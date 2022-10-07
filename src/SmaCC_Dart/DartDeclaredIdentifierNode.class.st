Class {
	#name : #DartDeclaredIdentifierNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'finalConstVarOrType',
		'declaredIdentifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartDeclaredIdentifierNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitDeclaredIdentifier: self
]

{ #category : #generated }
DartDeclaredIdentifierNode >> declaredIdentifier [

	^ declaredIdentifier
]

{ #category : #generated }
DartDeclaredIdentifierNode >> declaredIdentifier: aDartIdentifierNode [

	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: nil ].
	declaredIdentifier := aDartIdentifierNode.
	self declaredIdentifier notNil ifTrue: [ 
		self declaredIdentifier parent: self ]
]

{ #category : #generated }
DartDeclaredIdentifierNode >> finalConstVarOrType [

	^ finalConstVarOrType
]

{ #category : #generated }
DartDeclaredIdentifierNode >> finalConstVarOrType: anObject [

	self setParent: self finalConstVarOrType to: nil.
	finalConstVarOrType := anObject.
	self setParent: self finalConstVarOrType to: self
]

{ #category : #generated }
DartDeclaredIdentifierNode >> nodeVariables [

	^ #( #declaredIdentifier )
]

{ #category : #generated }
DartDeclaredIdentifierNode >> otherVariables [

	^ #( #finalConstVarOrType )
]
