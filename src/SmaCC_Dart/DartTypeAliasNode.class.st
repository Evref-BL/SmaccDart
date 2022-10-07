Class {
	#name : #DartTypeAliasNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifier',
		'type',
		'typeParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeAliasNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeAlias: self
]

{ #category : #generated }
DartTypeAliasNode >> nodeVariables [

	^ #( #type #typeParameters )
]

{ #category : #generated }
DartTypeAliasNode >> tokenVariables [

	^ #( #typeIdentifier )
]

{ #category : #generated }
DartTypeAliasNode >> type [

	^ type
]

{ #category : #generated }
DartTypeAliasNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]

{ #category : #generated }
DartTypeAliasNode >> typeIdentifier [

	^ typeIdentifier
]

{ #category : #generated }
DartTypeAliasNode >> typeIdentifier: aSmaCCToken [

	typeIdentifier := aSmaCCToken
]

{ #category : #generated }
DartTypeAliasNode >> typeParameters [

	^ typeParameters
]

{ #category : #generated }
DartTypeAliasNode >> typeParameters: aDartTypeParametersNode [

	self typeParameters notNil ifTrue: [ self typeParameters parent: nil ].
	typeParameters := aDartTypeParametersNode.
	self typeParameters notNil ifTrue: [ 
		self typeParameters parent: self ]
]
