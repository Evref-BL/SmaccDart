Class {
	#name : #DartEnumEntryNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'metadata',
		'identifiers',
		'argumentPart',
		'arguments',
		'typeArguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartEnumEntryNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitEnumEntry: self
]

{ #category : #generated }
DartEnumEntryNode >> argumentPart [

	^ argumentPart
]

{ #category : #generated }
DartEnumEntryNode >> argumentPart: anObject [

	self setParent: self argumentPart to: nil.
	argumentPart := anObject.
	self setParent: self argumentPart to: self
]

{ #category : #generated }
DartEnumEntryNode >> arguments [

	^ arguments
]

{ #category : #generated }
DartEnumEntryNode >> arguments: anObject [

	self setParent: self arguments to: nil.
	arguments := anObject.
	self setParent: self arguments to: self
]

{ #category : #generated }
DartEnumEntryNode >> compositeNodeVariables [

	^ #( #identifiers )
]

{ #category : #generated }
DartEnumEntryNode >> identifiers [

	^ identifiers
]

{ #category : #generated }
DartEnumEntryNode >> identifiers: anOrderedCollection [

	self setParents: self identifiers to: nil.
	identifiers := anOrderedCollection.
	self setParents: self identifiers to: self
]

{ #category : #'generated-initialize-release' }
DartEnumEntryNode >> initialize [
	super initialize.
	identifiers := OrderedCollection new: 2.
]

{ #category : #generated }
DartEnumEntryNode >> metadata [

	^ metadata
]

{ #category : #generated }
DartEnumEntryNode >> metadata: anObject [

	self setParent: self metadata to: nil.
	metadata := anObject.
	self setParent: self metadata to: self
]

{ #category : #generated }
DartEnumEntryNode >> nodeVariables [

	^ #( #typeArguments )
]

{ #category : #generated }
DartEnumEntryNode >> otherVariables [

	^ #( #metadata #argumentPart #arguments )
]

{ #category : #generated }
DartEnumEntryNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartEnumEntryNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]
