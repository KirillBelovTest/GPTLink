(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`GPTLink`"];


ClearAll["`*"]; 


GPTChatComplete::usage = 
"GPTChatComplete[prompt] complete given prompt. 
GPTChatComplete[chat, prompt] complete chat using given prompt."; 


GPTChatCompleteAsync::usage = 
"GPTChatCompleteAsync[prompt] complete given prompt in async mode. 
GPTChatCompleteAsync[chat, prompt] complete chat using given prompt in async mode.";  


GPTChatObject::usage = 
"GPTChatObject[\"text\"] symbolic chat representation in Wolfram Language."; 


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


$directory = ParentDirectory[DirectoryName[$InputFileName]]; 


$icon = Import[FileNameJoin[{$directory, "Images", "chatgpt-logo.png"}]]; 


Options[GPTChatObject] = {
	"Endpoint" -> "https://api.openai.com", 
	"Temperature" -> 0.7, 
	"User" -> Automatic, 
	"APIToken" :> SystemCredential["OPENAI_API_KEY"], 
	"Model" -> "gpt-4-turbo-preview", 
	"MaxTokens" -> 70000
}; 


GPTChatObject[OptionsPattern[]] := 
With[{
	chat = CreateDataStructure["HashTable"]
}, 
	chat["Insert", "Endpoint" -> OptionValue["Endpoint"]]; 
	chat["Insert", "Temperature" -> OptionValue["Temperature"]]; 
	chat["Insert", "User" -> OptionValue["User"]]; 
	chat["Insert", "APIToken" -> OptionValue["APIToken"]]; 
	chat["Insert", "MaxTokens" -> OptionValue["MaxTokens"]]; 
	chat["Insert", "Messages" -> CreateDataStructure["DynamicArray"]]; 

	GPTChatObject[chat]
]; 


GPTChatObject[system_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	With[{messages = chat["MessagesData"]}, 
		messages["Append", <|"role" -> "system", "content" -> system|>]
	]; 
	chat
]; 


GPTChatObject[chat_DataStructure][key_String] := 
chat["Lookup", key]; 


GPTChatObject[chat_DataStructure][index_Integer] := 
chat["Lookup", "Messages"]["Part", index]; 


GPTChatObject[chat_DataStructure]["Messages"] := 
Query[All, {"role", "content"}] @ chat["Lookup", "Messages"]["Elements"]; 


GPTChatObject[chat_DataStructure]["MessagesData"] := 
chat["Lookup", "Messages"]; 


GPTChatObject /: Append[GPTChatObject[chat_DataStructure], message_] := 
chat["Lookup", "Messages"]["Append", message]; 


GPTChatObject /: MakeBoxes[obj: GPTChatObject[chat_DataStructure], form: (StandardForm | TraditionalForm)] := 
Module[{above, below}, 
	above = {
		{BoxForm`SummaryItem[{"Length: ", Length[obj["Messages"]]}], SpanFromLeft}, 
		If[Length[obj["Messages"]] >= 1, {BoxForm`SummaryItem[{obj["Messages"][[-1, "role"]] <> ": ", obj["Messages"][[-1, "content"]]}], SpanFromLeft}, Nothing], 
		If[Length[obj["Messages"]] >= 2, {BoxForm`SummaryItem[{obj["Messages"][[-2, "role"]] <> ": ", obj["Messages"][[-2, "content"]]}], SpanFromLeft}, Nothing]
	}; 
	below = If[Length[#] > 2, 
		Function[m, {BoxForm`SummaryItem[{m["role"] <> ": ", m["content"]}], SpanFromLeft}] /@ Reverse[#[[ ;; -3]]], 
	{}]& @ obj["Messages"]; 
	
	BoxForm`ArrangeSummaryBox[GPTChatObject, obj, $icon, above, below, form, "Interpretable" -> Automatic]
];


Options[GPTChatComplete] = {
	"Endpoint" -> "https://api.openai.com", 
	"Temperature" -> 0.7, 
	"User" -> Automatic, 
	"APIToken" :> SystemCredential["OPENAI_API_KEY"], 
	"Model" -> "gpt-4-turbo-preview", 
	"MaxTokens" -> 70000
}; 


GPTChatComplete[chat_GPTChatObject, OptionsPattern[]] := 
Module[{url, endpoint = OptionValue["Endpoint"], headers, 
	apiToken = OptionValue["APIToken"], 
	messages, 
	model = OptionValue["Model"], 
	requestBody, 
	temperature = OptionValue["Temperature"], 
	request, response, responseBody, responseAssoc
}, 
	If[apiToken === Automatic, 
		apiToken = chat["APIToken"]
	]; 

	If[chat["APIToken"] === Automatic, 
		chat[[1]]["Insert", "APIToken" -> apiToken]
	]; 

	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
	headers = {
		"Authorization" -> "Bearer " <> apiToken, 
		"X-API-KEY" -> apiToken
	}; 
	
	messages = chat["Messages"]; 
	
	requestBody = ExportString[<|
		"model" -> model, 
		"messages" -> messages, 
		"temperature" -> temperature
	|>, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	response = URLRead[request]; 
	
	If[response["StatusCode"] === 200, 
		responseBody = ExportString[response["Body"], "Text", CharacterEncoding -> "UTF-8"]; 
		responseAssoc = ImportString[responseBody, "RawJSON"]; 
		If[AssociationQ[responseAssoc], 
			chat[[1]]["Insert", "ChatId" -> responseAssoc["id"]]; 
			chat[[1]]["Insert", "TotalTokens" -> responseAssoc["usage", "total_tokens"]]; 
			chat[[1]]["Lookup", "Messages"]["Append", responseAssoc[["choices", 1, "message"]]]; 
			chat, 
			$Failed
		], 
		$Failed
	]
]; 


GPTChatComplete[prompt_String, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	chat[[1]]["Lookup", "Messages"]["Append", <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatComplete[chat, opts]
]; 


GPTChatComplete[chat_GPTChatObject, prompt_String, opts: OptionsPattern[]] := (
	chat[[1]]["Lookup", "Messages"]["Append", <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatComplete[chat, opts]
); 


Options[GPTChatCompleteAsync] = Options[GPTChatComplete]; 


GPTChatCompleteAsync[chat_GPTChatObject, callback: _Function | _Symbol, OptionsPattern[]] := 
Module[{url, endpoint = OptionValue["Endpoint"], headers, 
	apiToken = OptionValue["APIToken"], 
	messages, 
	model = OptionValue["Model"], 
	requestBody, 
	temperature = OptionValue["Temperature"], 
	request
}, 
	If[apiToken === Automatic, 
		apiToken = chat["APIToken"]
	]; 

	If[chat["APIToken"] === Automatic, 
		chat[[1]]["Insert", "APIToken" -> apiToken]
	]; 

	url = URLBuild[{endpoint, "v1", "chat", "completions"}]; 
	
	headers = {
		"Authorization" -> "Bearer " <> apiToken, 
		"X-API-KEY" -> apiToken
	}; 
	
	messages = chat["Messages"]; 
	
	requestBody = ExportString[<|
		"model" -> model, 
		"messages" -> messages, 
		"temperature" -> temperature
	|>, "RawJSON", CharacterEncoding -> "UTF-8"]; 
	
	request = HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> headers, 
		"Body" -> requestBody
	|>]; 
	
	URLSubmit[request, 
		HandlerFunctions -> <|
  			"BodyReceived" -> Function[Module[{responseBody, responseAssoc}, 
				If[#["StatusCode"] === 200, 
					responseBody = ExportString[#["Body"], "String"]; 
					responseAssoc = ImportString[responseBody, "RawJSON", CharacterEncoding -> "UTF-8"]; 
					If[AssociationQ[responseAssoc], 
						chat[[1]]["Insert", "ChatId" -> responseAssoc["id"]]; 
						chat[[1]]["Insert", "TotalTokens" -> responseAssoc["usage", "total_tokens"]]; 
						chat[[1]]["Lookup", "Messages"]["Append", responseAssoc[["choices", 1, "message"]]]; 
						callback[chat], 
						$Failed
					], 
					$Failed
				]
			]]
		|>, 
		HandlerFunctionsKeys -> {"StatusCode", "Body"}
	]
]; 


GPTChatCompleteAsync[prompt_String, callack_, opts: OptionsPattern[]] := 
With[{chat = GPTChatObject[opts]}, 
	chat[[1]]["Lookup", "Messages"]["Append", <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatCompleteAsync[chat, callback, opts]
]; 


GPTChatCompleteAsync[chat_GPTChatObject, prompt_String, callback_, opts: OptionsPattern[]] := (
	chat[[1]]["Lookup", "Messages"]["Append", <|"role" -> "user", "content" -> prompt|>]; 
	GPTChatCompleteAsync[chat, callback, opts]
); 


(* ::Section:: *)
(*Package Footer*)


End[];


EndPackage[];
