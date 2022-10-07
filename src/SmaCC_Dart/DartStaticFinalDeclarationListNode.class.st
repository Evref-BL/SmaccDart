Class {
	#name : #DartStaticFinalDeclarationListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'staticFinalDeclaration',
		'nextStaticFinalDeclarations'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartStaticFinalDeclarationListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitStaticFinalDeclarationList: self
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> compositeNodeVariables [

	^ #( #nextStaticFinalDeclarations )
]

{ #category : #'generated-initialize-release' }
DartStaticFinalDeclarationListNode >> initialize [
	super initialize.
	nextStaticFinalDeclarations := OrderedCollection new: 2.
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> nextStaticFinalDeclarations [

	^ nextStaticFinalDeclarations
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> nextStaticFinalDeclarations: anOrderedCollection [

	self setParents: self nextStaticFinalDeclarations to: nil.
	nextStaticFinalDeclarations := anOrderedCollection.
	self setParents: self nextStaticFinalDeclarations to: self
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> nodeVariables [

	^ #( #staticFinalDeclaration )
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> staticFinalDeclaration [

	^ staticFinalDeclaration
]

{ #category : #generated }
DartStaticFinalDeclarationListNode >> staticFinalDeclaration: aDartStaticFinalDeclarationNode [

	self staticFinalDeclaration notNil ifTrue: [ 
		self staticFinalDeclaration parent: nil ].
	staticFinalDeclaration := aDartStaticFinalDeclarationNode.
	self staticFinalDeclaration notNil ifTrue: [ 
		self staticFinalDeclaration parent: self ]
]
