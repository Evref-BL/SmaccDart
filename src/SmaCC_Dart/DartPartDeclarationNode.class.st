Class {
	#name : #DartPartDeclarationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'identifiers',
		'stringLiteralWithoutInterpolation',
		'classDeclarations',
		'mixinDeclarations',
		'extensionDeclarations',
		'enumTypes',
		'typeAliases',
		'externalDeclarations',
		'functionDeclarations',
		'liststDeclarations'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartPartDeclarationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitPartDeclaration: self
]

{ #category : #generated }
DartPartDeclarationNode >> classDeclarations [

	^ classDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> classDeclarations: anOrderedCollection [

	self setParents: self classDeclarations to: nil.
	classDeclarations := anOrderedCollection.
	self setParents: self classDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> compositeNodeVariables [

	^ #( #identifiers #classDeclarations #extensionDeclarations
	     #typeAliases #externalDeclarations #functionDeclarations
	     #liststDeclarations )
]

{ #category : #generated }
DartPartDeclarationNode >> compositeTokenVariables [

	^ #( #mixinDeclarations #enumTypes )
]

{ #category : #generated }
DartPartDeclarationNode >> enumTypes [

	^ enumTypes
]

{ #category : #generated }
DartPartDeclarationNode >> enumTypes: anOrderedCollection [

	enumTypes := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> extensionDeclarations [

	^ extensionDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> extensionDeclarations: anOrderedCollection [

	self setParents: self extensionDeclarations to: nil.
	extensionDeclarations := anOrderedCollection.
	self setParents: self extensionDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> externalDeclarations [

	^ externalDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> externalDeclarations: anOrderedCollection [

	self setParents: self externalDeclarations to: nil.
	externalDeclarations := anOrderedCollection.
	self setParents: self externalDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> functionDeclarations [

	^ functionDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> functionDeclarations: anOrderedCollection [

	self setParents: self functionDeclarations to: nil.
	functionDeclarations := anOrderedCollection.
	self setParents: self functionDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartPartDeclarationNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartPartDeclarationNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
	classDeclarations := OrderedCollection new: 2.
	mixinDeclarations := OrderedCollection new: 2.
	extensionDeclarations := OrderedCollection new: 2.
	enumTypes := OrderedCollection new: 2.
	typeAliases := OrderedCollection new: 2.
	externalDeclarations := OrderedCollection new: 2.
	functionDeclarations := OrderedCollection new: 2.
	liststDeclarations := OrderedCollection new: 2.
]

{ #category : #generated }
DartPartDeclarationNode >> liststDeclarations [

	^ liststDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> liststDeclarations: anOrderedCollection [

	self setParents: self liststDeclarations to: nil.
	liststDeclarations := anOrderedCollection.
	self setParents: self liststDeclarations to: self
]

{ #category : #generated }
DartPartDeclarationNode >> mixinDeclarations [

	^ mixinDeclarations
]

{ #category : #generated }
DartPartDeclarationNode >> mixinDeclarations: anOrderedCollection [

	mixinDeclarations := anOrderedCollection
]

{ #category : #generated }
DartPartDeclarationNode >> stringLiteralWithoutInterpolation [

	^ stringLiteralWithoutInterpolation
]

{ #category : #generated }
DartPartDeclarationNode >> stringLiteralWithoutInterpolation: aSmaCCToken [

	stringLiteralWithoutInterpolation := aSmaCCToken
]

{ #category : #generated }
DartPartDeclarationNode >> tokenVariables [

	^ #( #stringLiteralWithoutInterpolation )
]

{ #category : #generated }
DartPartDeclarationNode >> typeAliases [

	^ typeAliases
]

{ #category : #generated }
DartPartDeclarationNode >> typeAliases: anOrderedCollection [

	self setParents: self typeAliases to: nil.
	typeAliases := anOrderedCollection.
	self setParents: self typeAliases to: self
]
