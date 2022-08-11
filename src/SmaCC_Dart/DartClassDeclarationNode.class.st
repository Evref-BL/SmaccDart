Class {
	#name : #DartClassDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'clazzToken',
		'type',
		'withToken'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartClassDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitClassDeclaration: self
]

{ #category : #generated }
DartClassDeclarationNode >> clazzToken [

	^ clazzToken
]

{ #category : #generated }
DartClassDeclarationNode >> clazzToken: aSmaCCToken [

	clazzToken := aSmaCCToken
]

{ #category : #generated }
DartClassDeclarationNode >> otherVariables [

	^ #( #type )
]

{ #category : #generated }
DartClassDeclarationNode >> tokenVariables [

	^ #( #clazzToken #withToken )
]

{ #category : #generated }
DartClassDeclarationNode >> type [

	^ type
]

{ #category : #generated }
DartClassDeclarationNode >> type: anOrderedCollection [

	self setParent: self type to: nil.
	type := anOrderedCollection.
	self setParent: self type to: self
]

{ #category : #generated }
DartClassDeclarationNode >> withToken [

	^ withToken
]

{ #category : #generated }
DartClassDeclarationNode >> withToken: aSmaCCToken [

	withToken := aSmaCCToken
]
