Class {
	#name : #DartTypeListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'type',
		'nextTypes'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeList: self
]

{ #category : #generated }
DartTypeListNode >> compositeNodeVariables [

	^ #( #nextTypes )
]

{ #category : #'generated-initialize-release' }
DartTypeListNode >> initialize [
	super initialize.
	nextTypes := OrderedCollection new: 2.
]

{ #category : #generated }
DartTypeListNode >> nextTypes [

	^ nextTypes
]

{ #category : #generated }
DartTypeListNode >> nextTypes: anOrderedCollection [

	self setParents: self nextTypes to: nil.
	nextTypes := anOrderedCollection.
	self setParents: self nextTypes to: self
]

{ #category : #generated }
DartTypeListNode >> nodeVariables [

	^ #( #type )
]

{ #category : #generated }
DartTypeListNode >> type [

	^ type
]

{ #category : #generated }
DartTypeListNode >> type: aDartTypeNode [

	self type notNil ifTrue: [ self type parent: nil ].
	type := aDartTypeNode.
	self type notNil ifTrue: [ self type parent: self ]
]
