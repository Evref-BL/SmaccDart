Class {
	#name : #DartInitializersNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'initializerListEntry',
		'InitializerListEntries'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartInitializersNode >> InitializerListEntries [

	^ InitializerListEntries
]

{ #category : #generated }
DartInitializersNode >> InitializerListEntries: anOrderedCollection [

	self setParent: self InitializerListEntries to: nil.
	InitializerListEntries := anOrderedCollection.
	self setParent: self InitializerListEntries to: self
]

{ #category : #generated }
DartInitializersNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitInitializers: self
]

{ #category : #'generated-initialize-release' }
DartInitializersNode >> initialize [
	super initialize.
	InitializerListEntries := OrderedCollection new: 2.
]

{ #category : #generated }
DartInitializersNode >> initializerListEntry [

	^ initializerListEntry
]

{ #category : #generated }
DartInitializersNode >> initializerListEntry: anObject [

	self setParent: self initializerListEntry to: nil.
	initializerListEntry := anObject.
	self setParent: self initializerListEntry to: self
]

{ #category : #generated }
DartInitializersNode >> otherVariables [

	^ #( #initializerListEntry #InitializerListEntries )
]
