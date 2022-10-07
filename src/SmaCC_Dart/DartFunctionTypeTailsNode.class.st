Class {
	#name : #DartFunctionTypeTailsNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'functionTypeTail',
		'functionTypeTails'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionTypeTailsNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionTypeTails: self
]

{ #category : #generated }
DartFunctionTypeTailsNode >> functionTypeTail [

	^ functionTypeTail
]

{ #category : #generated }
DartFunctionTypeTailsNode >> functionTypeTail: aDartFunctionTypeTailNode [

	self functionTypeTail notNil ifTrue: [ 
		self functionTypeTail parent: nil ].
	functionTypeTail := aDartFunctionTypeTailNode.
	self functionTypeTail notNil ifTrue: [ 
		self functionTypeTail parent: self ]
]

{ #category : #generated }
DartFunctionTypeTailsNode >> functionTypeTails [

	^ functionTypeTails
]

{ #category : #generated }
DartFunctionTypeTailsNode >> functionTypeTails: aDartProgramNode [

	self functionTypeTails notNil ifTrue: [ 
		self functionTypeTails parent: nil ].
	functionTypeTails := aDartProgramNode.
	self functionTypeTails notNil ifTrue: [ 
		self functionTypeTails parent: self ]
]

{ #category : #generated }
DartFunctionTypeTailsNode >> nodeVariables [

	^ #( #functionTypeTail #functionTypeTails )
]
