Class {
	#name : #DartFunctionTypeAliasNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'functionPrefix',
		'formalParameterPart'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionTypeAliasNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionTypeAlias: self
]

{ #category : #generated }
DartFunctionTypeAliasNode >> formalParameterPart [

	^ formalParameterPart
]

{ #category : #generated }
DartFunctionTypeAliasNode >> formalParameterPart: aDartFormalParameterPartNode [

	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: nil ].
	formalParameterPart := aDartFormalParameterPartNode.
	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: self ]
]

{ #category : #generated }
DartFunctionTypeAliasNode >> functionPrefix [

	^ functionPrefix
]

{ #category : #generated }
DartFunctionTypeAliasNode >> functionPrefix: aDartProgramNode [

	self functionPrefix notNil ifTrue: [ self functionPrefix parent: nil ].
	functionPrefix := aDartProgramNode.
	self functionPrefix notNil ifTrue: [ 
		self functionPrefix parent: self ]
]

{ #category : #generated }
DartFunctionTypeAliasNode >> nodeVariables [

	^ #( #functionPrefix #formalParameterPart )
]
