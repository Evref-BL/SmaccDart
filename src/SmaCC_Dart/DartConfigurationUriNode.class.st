Class {
	#name : #DartConfigurationUriNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'uriTest',
		'uriConfigured'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConfigurationUriNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConfigurationUri: self
]

{ #category : #generated }
DartConfigurationUriNode >> nodeVariables [

	^ #( #uriTest #uriConfigured )
]

{ #category : #generated }
DartConfigurationUriNode >> uriConfigured [

	^ uriConfigured
]

{ #category : #generated }
DartConfigurationUriNode >> uriConfigured: aDartUriNode [

	self uriConfigured notNil ifTrue: [ self uriConfigured parent: nil ].
	uriConfigured := aDartUriNode.
	self uriConfigured notNil ifTrue: [ self uriConfigured parent: self ]
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
