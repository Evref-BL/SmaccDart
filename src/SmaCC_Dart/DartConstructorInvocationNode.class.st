Class {
	#name : #DartConstructorInvocationNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeName',
		'typeArguments',
		'arguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartConstructorInvocationNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitConstructorInvocation: self
]

{ #category : #generated }
DartConstructorInvocationNode >> arguments [

	^ arguments
]

{ #category : #generated }
DartConstructorInvocationNode >> arguments: anObject [

	self setParent: self arguments to: nil.
	arguments := anObject.
	self setParent: self arguments to: self
]

{ #category : #generated }
DartConstructorInvocationNode >> nodeVariables [

	^ #( #typeName #typeArguments )
]

{ #category : #generated }
DartConstructorInvocationNode >> otherVariables [

	^ #( #arguments )
]

{ #category : #generated }
DartConstructorInvocationNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartConstructorInvocationNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]

{ #category : #generated }
DartConstructorInvocationNode >> typeName [

	^ typeName
]

{ #category : #generated }
DartConstructorInvocationNode >> typeName: aDartTypeNameNode [

	self typeName notNil ifTrue: [ self typeName parent: nil ].
	typeName := aDartTypeNameNode.
	self typeName notNil ifTrue: [ self typeName parent: self ]
]
