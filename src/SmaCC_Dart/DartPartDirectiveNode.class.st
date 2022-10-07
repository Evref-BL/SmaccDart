Class {
	#name : #DartPartDirectiveNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'uri'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPartDirectiveNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPartDirective: self
]

{ #category : #generated }
DartPartDirectiveNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartPartDirectiveNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartPartDirectiveNode >> nodeVariables [

	^ #( #uri )
]

{ #category : #generated }
DartPartDirectiveNode >> otherVariables [

	^ #( #metadata )
]

{ #category : #generated }
DartPartDirectiveNode >> uri [

	^ uri
]

{ #category : #generated }
DartPartDirectiveNode >> uri: aDartUriNode [

	self uri notNil ifTrue: [ self uri parent: nil ].
	uri := aDartUriNode.
	self uri notNil ifTrue: [ self uri parent: self ]
]
