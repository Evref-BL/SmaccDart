Class {
	#name : #DartTypeNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifier',
		'typeParameter',
		'nextTypeParameters',
		'functionType',
		'typeNotFunction'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitType: self
]

{ #category : #generated }
DartTypeNode >> compositeNodeVariables [

	^ #( #nextTypeParameters )
]

{ #category : #generated }
DartTypeNode >> functionType [

	^ functionType
]

{ #category : #generated }
DartTypeNode >> functionType: aDartProgramNode [

	self functionType notNil ifTrue: [ self functionType parent: nil ].
	functionType := aDartProgramNode.
	self functionType notNil ifTrue: [ self functionType parent: self ]
]

{ #category : #'generated-initialize-release' }
DartTypeNode >> initialize [
	super initialize.
	nextTypeParameters := OrderedCollection new: 2.
]

{ #category : #generated }
DartTypeNode >> nextTypeParameters [

	^ nextTypeParameters
]

{ #category : #generated }
DartTypeNode >> nextTypeParameters: anOrderedCollection [

	self setParents: self nextTypeParameters to: nil.
	nextTypeParameters := anOrderedCollection.
	self setParents: self nextTypeParameters to: self
]

{ #category : #generated }
DartTypeNode >> nodeVariables [

	^ #( #typeParameter #functionType )
]

{ #category : #generated }
DartTypeNode >> otherVariables [

	^ #( #typeNotFunction )
]

{ #category : #generated }
DartTypeNode >> tokenVariables [

	^ #( #typeIdentifier )
]

{ #category : #generated }
DartTypeNode >> typeIdentifier [

	^ typeIdentifier
]

{ #category : #generated }
DartTypeNode >> typeIdentifier: aSmaCCToken [

	typeIdentifier := aSmaCCToken
]

{ #category : #generated }
DartTypeNode >> typeNotFunction [

	^ typeNotFunction
]

{ #category : #generated }
DartTypeNode >> typeNotFunction: anObject [

	self setParent: self typeNotFunction to: nil.
	typeNotFunction := anObject.
	self setParent: self typeNotFunction to: self
]

{ #category : #generated }
DartTypeNode >> typeParameter [

	^ typeParameter
]

{ #category : #generated }
DartTypeNode >> typeParameter: aDartTypeParameterNode [

	self typeParameter notNil ifTrue: [ self typeParameter parent: nil ].
	typeParameter := aDartTypeParameterNode.
	self typeParameter notNil ifTrue: [ self typeParameter parent: self ]
]
