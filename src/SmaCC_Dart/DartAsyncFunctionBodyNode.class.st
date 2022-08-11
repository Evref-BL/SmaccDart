Class {
	#name : #DartAsyncFunctionBodyNode,
	#superclass : #DartFunctionBodyNode,
	#instVars : [
		'asyncToken',
		'timesToken',
		'syncToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartAsyncFunctionBodyNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitAsyncFunctionBody: self
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> asyncToken [

	^ asyncToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> asyncToken: aSmaCCToken [

	asyncToken := aSmaCCToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> syncToken [

	^ syncToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> syncToken: aSmaCCToken [

	syncToken := aSmaCCToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> timesToken [

	^ timesToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> timesToken: aSmaCCToken [

	timesToken := aSmaCCToken
]

{ #category : #generated }
DartAsyncFunctionBodyNode >> tokenVariables [

	^ #( #asyncToken #timesToken #syncToken #equalGreaterThanToken
	     #semicolonToken )
]
