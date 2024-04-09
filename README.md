# GPTLink

## Installation

```mathematica
PacletInstall["KirillBelov/GPTLink"]
```

## Import

```mathematica
Get["KirillBelov`GTPLink`"]
```

## Functions

### GPTChatObject

Symbolic chat representation.

```mathematica
GPTChatObject[]

GPTChatObject["System info for the bot"]

GPTChatObject[
    "APIToken" -> SystemCreadential["OPENAI_API_KEY"], 
    "Endpoint" -> "https://api.openai.com", 
    "MaxTokens" -> 70000, 
    "Model" -> "gpt-4-turo-preview", 
    "Temperature" -> 0.7, 
    "Tools" -> {pluginFunc1, pluginFunc2, ...}, 
    "ToolsChoice" -> "auto" | "none" | pluginFunc1
]
```

### GPTChatComplete

Complete the given chat. 

```mathematica
GPTChatComplete["prompt"] 

GPTChatComplete[chatObject] 

GPTChatComplete[chatObject, "prompt"]

GPTChatComplete[..., chatObjectOptions]
```

### GPTChatCompleteAsync

Async chat completion.  
When the body is received, a callback executes. 

```mathematica
GPTChatCompleteAsync["prompt", callbackFunction] 

GPTChatComplete[chatObject, callbackFunction] 

GPTChatComplete[chatObject, "prompt", callbackFunction]

GPTChatComplete[..., callbackFunction, chatObjectOptions]
```

## Chat properties

The chat is a mutable object.  
It has properties that give access to chat messages and additional information. 

```mathematica
chat = GPTChatObject[]; 
GPTChatComplete[chat, "Hi!"];

chat["Messages"]
(*Out[] = {
    <|"role" -> "user", "content" -> "Hi!"|>, 
    <|"role" -> "assistant", "content" -> "Hello! How can I assist you today?"|>
}*)
```

## Plugins

As a plugin, the chat expects a simple function in the Wolfram Language. 
It must have arguments of type String and a usage description. 
For example a simple function that gives access to Wolfram Alpha:

```mathematica
wolframAlpha::usage = 
"wolframAlpha[query] returns answer from WolframAlpha using query."; 

wolframAlpha[query_String] := 
WolframAlpha[TextTranslation[query, "English"], "ShortAnswer"]
```

Now let's create a chat + plugin and let's make request to OpenAI:

```mathematica
chat = GPTChatObject[
    "You are bot with access to WolframAlpha with function wolframAlpha(query).", 
    "Tools" -> {wolframAlpha}
]; 

GPTChatComplete[chat, "Hi, what is date today?"]["Messages"]
(*{
    <|"role" -> "system",    "content" -> "You are bot with access to WolframAlpha with function wolframAlpha(query)."|>, 
    <|"role" -> "user",      "content" -> "Hi, what is date today?"|>, 
    <|"role" -> "assistant", "content" -> Null, "tool_calls" -> {
        <|"id" -> "call_mlXTrFY070v2kdJRaNgjcUBu", "type" -> "function", "function" -> <|
            "name" -> "wolframAlpha", 
            "arguments" -> "{\"query\":\"current date\"}"|>|>}|>, 
    <|"role" -> "tool",      "content" -> "Tuesday, April 9, 2024", 
        "name" -> "wolframAlpha", "tool_call_id" -> "call_mlXTrFY070v2kdJRaNgjcUBu"|>, 
    <|"role" -> "assistant", "content" -> "Today is Tuesday, April 9, 2024."|>
}*)
```
