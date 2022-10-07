Class {
	#name : #DartListLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'elements',
		'typeArguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartListLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitListLiteral: self
]

{ #category : #generated }
DartListLiteralNode >> elements [

	^ elements
]

{ #category : #generated }
DartListLiteralNode >> elements: aDartElementsNode [

	self elements notNil ifTrue: [ self elements parent: nil ].
	elements := aDartElementsNode.
	self elements notNil ifTrue: [ self elements parent: self ]
]

{ #category : #generated }
DartListLiteralNode >> nodeVariables [

	^ #( #elements #typeArguments )
]

{ #category : #generated }
DartListLiteralNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartListLiteralNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]
