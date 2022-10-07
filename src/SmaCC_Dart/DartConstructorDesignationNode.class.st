Class {
	#name : #DartConstructorDesignationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeName',
		'typeArguments',
		'identifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstructorDesignationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstructorDesignation: self
]

{ #category : #generated }
DartConstructorDesignationNode >> identifier [

	^ identifier
]

{ #category : #generated }
DartConstructorDesignationNode >> identifier: aDartIdentifierNode [

	self identifier notNil ifTrue: [ self identifier parent: nil ].
	identifier := aDartIdentifierNode.
	self identifier notNil ifTrue: [ self identifier parent: self ]
]

{ #category : #generated }
DartConstructorDesignationNode >> nodeVariables [

	^ #( #typeName #typeArguments #identifier )
]

{ #category : #generated }
DartConstructorDesignationNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartConstructorDesignationNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]

{ #category : #generated }
DartConstructorDesignationNode >> typeName [

	^ typeName
]

{ #category : #generated }
DartConstructorDesignationNode >> typeName: aDartTypeNameNode [

	self typeName notNil ifTrue: [ self typeName parent: nil ].
	typeName := aDartTypeNameNode.
	self typeName notNil ifTrue: [ self typeName parent: self ]
]
