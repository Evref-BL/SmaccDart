Class {
	#name : #DartFunctionTypeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeNotFunction',
		'functionTypeTails'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionTypeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionType: self
]

{ #category : #generated }
DartFunctionTypeNode >> functionTypeTails [

	^ functionTypeTails
]

{ #category : #generated }
DartFunctionTypeNode >> functionTypeTails: aDartProgramNode [

	self functionTypeTails notNil ifTrue: [ 
		self functionTypeTails parent: nil ].
	functionTypeTails := aDartProgramNode.
	self functionTypeTails notNil ifTrue: [ 
		self functionTypeTails parent: self ]
]

{ #category : #generated }
DartFunctionTypeNode >> nodeVariables [

	^ #( #functionTypeTails )
]

{ #category : #generated }
DartFunctionTypeNode >> otherVariables [

	^ #( #typeNotFunction )
]

{ #category : #generated }
DartFunctionTypeNode >> typeNotFunction [

	^ typeNotFunction
]

{ #category : #generated }
DartFunctionTypeNode >> typeNotFunction: anObject [

	self setParent: self typeNotFunction to: nil.
	typeNotFunction := anObject.
	self setParent: self typeNotFunction to: self
]
