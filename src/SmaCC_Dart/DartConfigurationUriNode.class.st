Class {
	#name : #DartConfigurationUriNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'ifToken',
		'leftParenToken',
		'uriTest',
		'rightParenToken',
		'uri'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConfigurationUriNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConfigurationUri: self
]

{ #category : #generated }
DartConfigurationUriNode >> ifToken [

	^ ifToken
]

{ #category : #generated }
DartConfigurationUriNode >> ifToken: aSmaCCToken [

	ifToken := aSmaCCToken
]

{ #category : #generated }
DartConfigurationUriNode >> leftParenToken [

	^ leftParenToken
]

{ #category : #generated }
DartConfigurationUriNode >> leftParenToken: aSmaCCToken [

	leftParenToken := aSmaCCToken
]

{ #category : #generated }
DartConfigurationUriNode >> nodeVariables [

	^ #( #uriTest #uri )
]

{ #category : #generated }
DartConfigurationUriNode >> rightParenToken [

	^ rightParenToken
]

{ #category : #generated }
DartConfigurationUriNode >> rightParenToken: aSmaCCToken [

	rightParenToken := aSmaCCToken
]

{ #category : #generated }
DartConfigurationUriNode >> tokenVariables [

	^ #( #ifToken #leftParenToken #rightParenToken )
]

{ #category : #generated }
DartConfigurationUriNode >> uri [

	^ uri
]

{ #category : #generated }
DartConfigurationUriNode >> uri: aDartUriNode [

	self uri notNil ifTrue: [ self uri parent: nil ].
	uri := aDartUriNode.
	self uri notNil ifTrue: [ self uri parent: self ]
]

{ #category : #generated }
DartConfigurationUriNode >> uriTest [

	^ uriTest
]

{ #category : #generated }
DartConfigurationUriNode >> uriTest: aDartUriTestNode [

	self uriTest notNil ifTrue: [ self uriTest parent: nil ].
	uriTest := aDartUriTestNode.
	self uriTest notNil ifTrue: [ self uriTest parent: self ]
]
