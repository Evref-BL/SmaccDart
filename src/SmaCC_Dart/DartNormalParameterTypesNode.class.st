Class {
	#name : #DartNormalParameterTypesNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'normalParameterType',
		'nextNormalParameterTypes'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartNormalParameterTypesNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitNormalParameterTypes: self
]

{ #category : #generated }
DartNormalParameterTypesNode >> compositeNodeVariables [

	^ #( #nextNormalParameterTypes )
]

{ #category : #'generated-initialize-release' }
DartNormalParameterTypesNode >> initialize [
	super initialize.
	nextNormalParameterTypes := OrderedCollection new: 2.
]

{ #category : #generated }
DartNormalParameterTypesNode >> nextNormalParameterTypes [

	^ nextNormalParameterTypes
]

{ #category : #generated }
DartNormalParameterTypesNode >> nextNormalParameterTypes: anOrderedCollection [

	self setParents: self nextNormalParameterTypes to: nil.
	nextNormalParameterTypes := anOrderedCollection.
	self setParents: self nextNormalParameterTypes to: self
]

{ #category : #generated }
DartNormalParameterTypesNode >> nodeVariables [

	^ #( #normalParameterType )
]

{ #category : #generated }
DartNormalParameterTypesNode >> normalParameterType [

	^ normalParameterType
]

{ #category : #generated }
DartNormalParameterTypesNode >> normalParameterType: aDartNormalParameterTypeNode [

	self normalParameterType notNil ifTrue: [ 
		self normalParameterType parent: nil ].
	normalParameterType := aDartNormalParameterTypeNode.
	self normalParameterType notNil ifTrue: [ 
		self normalParameterType parent: self ]
]
