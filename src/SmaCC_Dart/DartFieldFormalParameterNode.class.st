Class {
	#name : #DartFieldFormalParameterNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifier',
		'formalParameterPart',
		'finalConstVarOrType'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFieldFormalParameterNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFieldFormalParameter: self
]

{ #category : #generated }
DartFieldFormalParameterNode >> finalConstVarOrType [

	^ finalConstVarOrType
]

{ #category : #generated }
DartFieldFormalParameterNode >> finalConstVarOrType: anObject [

	self setParent: self finalConstVarOrType to: nil.
	finalConstVarOrType := anObject.
	self setParent: self finalConstVarOrType to: self
]

{ #category : #generated }
DartFieldFormalParameterNode >> formalParameterPart [

	^ formalParameterPart
]

{ #category : #generated }
DartFieldFormalParameterNode >> formalParameterPart: aDartFormalParameterPartNode [

	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: nil ].
	formalParameterPart := aDartFormalParameterPartNode.
	self formalParameterPart notNil ifTrue: [ 
		self formalParameterPart parent: self ]
]

{ #category : #generated }
DartFieldFormalParameterNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartFieldFormalParameterNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartFieldFormalParameterNode >> nodeVariables [

	^ #( #identifier #formalParameterPart )
]

{ #category : #generated }
DartFieldFormalParameterNode >> otherVariables [

	^ #( #finalConstVarOrType )
]
