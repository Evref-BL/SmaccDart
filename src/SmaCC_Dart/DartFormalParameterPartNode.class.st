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
DartFormalParameterPartNode >> formalParameterList: anObject [

	self setParent: self formalParameterList to: nil.
	formalParameterList := anObject.
	self setParent: self formalParameterList to: self
]

{ #category : #generated }
DartFormalParameterPartNode >> nodeVariables [

	^ #( #typeParameters )
]

{ #category : #generated }
DartFormalParameterPartNode >> otherVariables [

	^ #( #formalParameterList )
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
