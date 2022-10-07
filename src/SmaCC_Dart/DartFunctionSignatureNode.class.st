Class {
	#name : #DartFunctionSignatureNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifierNotFUNCTION',
		'formalParameterPart',
		'type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionSignatureNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionSignature: self
]

{ #category : #generated }
DartFunctionSignatureNode >> formalParameterPart [

	^ formalParameterPart
]

{ #category : #generated }
DartFunctionSignatureNode >> formalParameterPart: aDartFormalParameterPartNode [

	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: nil ].
	formalParameterPart := aDartFormalParameterPartNode.
	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: self ]
]

{ #category : #generated }
DartFunctionSignatureNode >> identifierNotFUNCTION [

	^ identifierNotFUNCTION
]

{ #category : #generated }
DartFunctionSignatureNode >> identifierNotFUNCTION: aSmaCCToken [

	identifierNotFUNCTION := aSmaCCToken
]

{ #category : #generated }
DartFunctionSignatureNode >> nodeVariables [

	^ #( #formalParameterPart #type )
]

{ #category : #generated }
DartFunctionSignatureNode >> tokenVariables [

	^ #( #identifierNotFUNCTION )
]

{ #category : #generated }
DartFunctionSignatureNode >> type [

	^ type
]

{ #category : #generated }
DartFunctionSignatureNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
