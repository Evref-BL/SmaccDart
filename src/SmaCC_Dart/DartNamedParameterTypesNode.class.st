Class {
	#name : #DartNamedParameterTypesNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'namedParameterType',
		'nextNamedParameterTypes'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNamedParameterTypesNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNamedParameterTypes: self
]

{ #category : #generated }
DartNamedParameterTypesNode >> compositeNodeVariables [

	^ #( #nextNamedParameterTypes )
]

{ #category : #'generated-initialize-release' }
DartNamedParameterTypesNode >> initialize [
	super initialize.
	nextNamedParameterTypes := OrderedCollection new: 2.
]

{ #category : #generated }
DartNamedParameterTypesNode >> namedParameterType [

	^ namedParameterType
]

{ #category : #generated }
DartNamedParameterTypesNode >> namedParameterType: aDartNamedParameterTypeNode [

	self namedParameterType notNil ifTrue: [ 
		self namedParameterType parent: nil ].
	namedParameterType := aDartNamedParameterTypeNode.
	self namedParameterType notNil ifTrue: [ 
		self namedParameterType parent: self ]
]

{ #category : #generated }
DartNamedParameterTypesNode >> nextNamedParameterTypes [

	^ nextNamedParameterTypes
]

{ #category : #generated }
DartNamedParameterTypesNode >> nextNamedParameterTypes: anOrderedCollection [

	self setParents: self nextNamedParameterTypes to: nil.
	nextNamedParameterTypes := anOrderedCollection.
	self setParents: self nextNamedParameterTypes to: self
]

{ #category : #generated }
DartNamedParameterTypesNode >> nodeVariables [

	^ #( #namedParameterType )
]
