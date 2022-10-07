Class {
	#name : #DartFunctionFormalParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifierNotFUNCTION',
		'formalParameterPart',
		'Type'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionFormalParameterNode >> Type [

	^ Type
]

{ #category : #generated }
DartFunctionFormalParameterNode >> Type: aDartTypeNode [

	self Type notNil ifTrue: [ self Type parent: nil ].
	Type := aDartTypeNode.
	self Type notNil ifTrue: [ self Type parent: self ]
]

{ #category : #generated }
DartFunctionFormalParameterNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionFormalParameter: self
]

{ #category : #generated }
DartFunctionFormalParameterNode >> formalParameterPart [

	^ formalParameterPart
]

{ #category : #generated }
DartFunctionFormalParameterNode >> formalParameterPart: aDartFormalParameterPartNode [

	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: nil ].
	formalParameterPart := aDartFormalParameterPartNode.
	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: self ]
]

{ #category : #generated }
DartFunctionFormalParameterNode >> identifierNotFUNCTION [

	^ identifierNotFUNCTION
]

{ #category : #generated }
DartFunctionFormalParameterNode >> identifierNotFUNCTION: aSmaCCToken [

	identifierNotFUNCTION := aSmaCCToken
]

{ #category : #generated }
DartFunctionFormalParameterNode >> nodeVariables [

	^ #( #formalParameterPart #Type )
]

{ #category : #generated }
DartFunctionFormalParameterNode >> tokenVariables [

	^ #( #identifierNotFUNCTION )
]
