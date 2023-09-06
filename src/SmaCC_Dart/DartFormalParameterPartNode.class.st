Class {
	#name : #DartFormalParameterPartNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'formalParameterList',
		'typeParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFormalParameterPartNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFormalParameterPart: self
]

{ #category : #generated }
DartFormalParameterPartNode >> formalParameterList [

	^ formalParameterList
]

{ #category : #generated }
DartFormalParameterPartNode >> formalParameterList: aDartProgramNode [

	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: nil ].
	formalParameterList := aDartProgramNode.
	self formalParameterList notNil ifTrue: [ 
		self formalParameterList parent: self ]
]

{ #category : #generated }
DartFormalParameterPartNode >> nodeVariables [

	^ #( #formalParameterList #typeParameters )
]

{ #category : #generated }
DartFormalParameterPartNode >> typeParameters [

	^ typeParameters
]

{ #category : #generated }
DartFormalParameterPartNode >> typeParameters: aDartTypeParametersNode [

	self typeParameters notNil ifTrue: [ self typeParameters parent: nil ].
	typeParameters := aDartTypeParametersNode.
	self typeParameters notNil ifTrue: [ 
		self typeParameters parent: self ]
]
