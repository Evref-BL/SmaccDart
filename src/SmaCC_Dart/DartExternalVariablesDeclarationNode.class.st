Class {
	#name : #DartExternalVariablesDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'type',
		'identifierList'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartExternalVariablesDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitExternalVariablesDeclaration: self
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> identifierList [

	^ identifierList
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> identifierList: aDartIdentifierListNode [

	self identifierList notNil ifTrue: [ self identifierList parent: nil ].
	identifierList := aDartIdentifierListNode.
	self identifierList notNil ifTrue: [ 
		self identifierList parent: self ]
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> nodeVariables [

	^ #( #identifierList )
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> otherVariables [

	^ #( #type )
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> type [

	^ type
]

{ #category : #generated }
DartExternalVariablesDeclarationNode >> type: anObject [

	self setParent: self type to: nil.
	type := anObject.
	self setParent: self type to: self
]
