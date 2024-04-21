(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`GPTLink`", {"KirillBelov`Objects`"}];


ClearAll["`*"]; 


GPTChatComplete::usage = 
"GPTChatComplete[chat] complete given chat. 
GPTChatCompleteAsync[prompt] complete given prompt. 
GPTChatCompleteAsync[chat, prompt] complete chat using given prompt."; 


GPTChatCompleteAsync::usage = 
"GPTChatCompleteAsync[chat, callback] complete given chat in async mode. 
GPTChatCompleteAsync[prompt, callback] complete given prompt in async mode. 
GPTChatCompleteAsync[chat, prompt, callback] complete chat using given prompt in async mode."; 


GPTChatObject::usage = 
"GPTChatObject[] symbolic chat representation in Wolfram Language."; 


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


$directory = ParentDirectory[DirectoryName[$InputFileName]]; 


$icon = Import[FileNameJoin[{$directory, "Images", "chatgpt-logo.png"}]]; 


CreateType[GPTChatObject, {
	"Icon" -> $icon, 
	"Endpoint" -> "https://api.openai.com", 
	"Temperature" -> 0.7, 
	"User", 
	"APIToken" :> SystemCredential["OPENAI_API_KEY"], 
	"Model" -> "gpt-4-turbo-preview", 
	"MaxTokens" -> 70000, 
	"TotalTokens" -> 0, 
	"Tools" -> {}, 
	"ToolHandler" -> defaultToolHandler,
	"ToolFunction" -> defaultToolFunction,
	"ToolChoice" -> "auto", 
	"Messages" -> {}, 
	"Logger" -> None
}]; 


GPTChatObject[system_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	chat["Messages"] = Append[chat["Messages"], <|
		"role" -> "system", 
		"date" -> Now,
		"content" -> system
	|>]; 
	chat
]; 


GPTChatObject /: Append[chat_GPTChatObject, message_Association?AssociationQ] := 
(chat["Messages"] = Append[chat["Messages"], message]; chat); 


Options[GPTChatCompleteAsync] = {
	"Endpoint" -> Automatic, 
	"Temperature" -> Automatic, 
	"User" -> Automatic, 
	"APIToken" -> Automatic, 
	"Model" -> Automatic, 
	"MaxTokens" -> Automatic, 
	"Tools" -> Automatic, 
	"ToolChoice" -> Automatic, 
	"ToolFunction" -> Automatic,
	"ToolHandler" -> Automatic,
	"Logger" -> Automatic
}; 


GPTChatCompleteAsync::err = 
"`1`"; 


GPTChatCompleteAsync[chat_GPTChatObject, callback: _Function | _Symbol, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
Module[{ 
	endpoint = ifAuto[OptionValue["Endpoint"], chat["Endpoint"]],  
	apiToken = ifAuto[OptionValue["APIToken"], chat["APIToken"]], 
	model = ifAuto[OptionValue["Model"], chat["Model"]], 
	temperature = ifAuto[OptionValue["Temperature"], chat["Temperature"]], 
	tools = ifAuto[OptionValue["Tools"], chat["Tools"]], 
	toolFunction = ifAuto[OptionValue["ToolFunction"], chat["ToolFunction"]], 
	toolChoice = ifAuto[OptionValue["ToolChoice"], chat["ToolChoice"]], 
	maxTokens = ifAuto[OptionValue["MaxTokens"], chat["MaxTokens"]], 
	logger = ifAuto[OptionValue["Logger"], chat["Logger"]],
	toolHandler = ifAuto[OptionValue["ToolHandler"], chat["ToolHandler"]],
	url, 
	headers, 
	messages, 
	requestAssoc, 
	requestBody, 
	request
}, 
	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
	headers = {
		"Authorization" -> "Bearer " <> apiToken, 
		"X-API-KEY" -> apiToken
	}; 
	
	messages = chat["Messages"]; 
	
	requestAssoc = <|
		"model" -> model, 
		"messages" -> sanitaze[messages], 
		"temperature" -> temperature, 
		If[# === Nothing, Nothing, "tools" -> #] &@ toolFunction[tools], 
		If[Length[tools] > 0, "tool_choice" -> functionChoice[toolChoice], Nothing]
	|>; 

	requestBody = ExportString[requestAssoc, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	With[{$request = request, $logger = logger, $requestAssoc = requestAssoc}, 
		URLSubmit[$request, 
			HandlerFunctions -> <|
				"HeadersReceived" -> Function[$logger[<|"Body" -> $requestAssoc, "Event" -> "RequestBody"|>]], 
				"BodyReceived" -> Function[Module[{responseBody, responseAssoc}, 
					If[#["StatusCode"] === 200, 
						responseBody = ExportString[#["Body"], "String"]; 
						responseAssoc = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 

						$logger[<|"Body" -> responseAssoc, "Event" -> "ResponseBody"|>]; 

						If[AssociationQ[responseAssoc], 
							chat["ChatId"] = responseAssoc["id"]; 
							chat["TotalTokens"] = responseAssoc["usage", "total_tokens"]; 
							Append[chat, Join[responseAssoc[["choices", 1, "message"]], <|"date" -> Now|>] ]; 

							If[KeyExistsQ[chat["Messages"][[-1]], "tool_calls"], 
								Module[{
									$result = toolHandler[chat["Messages"][[-1]]]
								}, 
								

									If[StringQ[$result], 
										Append[chat, <|
											"role" -> "tool", 
											"content" -> $result, 
											"name" -> chat["Messages"][[-1, "tool_calls", 1, "function", "name"]], 
											"tool_call_id" -> chat["Messages"][[-1, "tool_calls", 1, "id"]],
											"date" -> Now
										|>]; 

										If[secondCall === GPTChatComplete, 
											secondCall[chat, opts], 
										(*Else*)
											secondCall[chat, callback, secondCall, opts]
										], 
									(*Else*)
										Message[GPTChatCompleteAsync::err, $result]; $Failed		
									];
								], 
								callback[chat], 
							(*Else*)
								Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
							], 
						(*Else*)
							Message[GPTChatCompleteAsync::err, responseAssoc]; $Failed
						], 
						$Failed
					]
				]]
			|>, 
			HandlerFunctionsKeys -> {"StatusCode", "Body", "Headers"}
		]
	]
]; 


GPTChatCompleteAsync[chat_GPTChatObject, prompt_String, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := (
	Append[chat, <|"role" -> "user", "content" -> prompt, "date"->Now|>]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
); 


GPTChatCompleteAsync[prompt_String, callback: _Symbol | _Function, 
	secondCall: GPTChatComplete | GPTChatCompleteAsync: GPTChatCompleteAsync, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, 
	Append[chat, <|"role" -> "user", "content" -> prompt, "date"->Now|>]; 
	GPTChatCompleteAsync[chat, callback, secondCall, opts]
]; 


Options[GPTChatComplete] = Options[GPTChatCompleteAsync]; 


GPTChatComplete[chat_GPTChatObject, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[chat_GPTChatObject, prompt_String, opts: OptionsPattern[]] := 
(TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat); 


GPTChatComplete[prompt_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[]}, TaskWait[GPTChatCompleteAsync[chat, prompt, Identity, GPTChatComplete, opts]]; chat]; 


(* ::Sction:: *)
(*Internal*)


ifAuto[Automatic, value_] := value; 


ifAuto[value_, _] := value; 

defaultToolHandler[message_] := Module[{func = message[["tool_calls", 1, "function", "name"]] // ToExpression},
	Apply[$func] @ Values @ ImportString[ImportString[
										message["Messages"][["tool_calls", 1, "function", "arguments"]], 
										"Text"], "RawJSON", CharacterEncoding -> "UTF-8"
									]
]


defaultToolFunction[function_Symbol] := 
<|
	"type" -> "function", 
	"function" -> <|
		"name" -> SymbolName[function], 
		"description" -> function::usage, 
		"parameters" -> <|
			"type" -> "object", 
			"properties" -> Apply[Association @* List] @ (
				(
					First[First[DownValues[function]]] /. 
					Verbatim[HoldPattern][function[args___]] :> Hold[args]
				) /. 
				Verbatim[Pattern][$s_Symbol, Verbatim[Blank][$t_]] :> 
				ToString[Unevaluated[$s]] -> <|
					"type" -> ToLowerCase[ToString[$t]], 
					"description" -> ToString[Unevaluated[$s]]
				|>
			)
		|>
	|>
|>; 

defaultToolFunction[list_List] := If[Length[list] > 0, Map[defaultToolFunction] @ tools, Nothing]

defaultToolFunction[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[function_Symbol] := 
<|"type" -> "function", "function" -> <|"name" -> SymbolName[function]|>|>; 


functionChoice[Automatic | "auto"] := 
"auto"; 


functionChoice[assoc_Association?AssociationQ] := 
assoc; 


functionChoice[_] := 
"none"; 

sanitaze[list_List] :=  Function[message, KeyDrop[message, "date"] ] /@ list 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
