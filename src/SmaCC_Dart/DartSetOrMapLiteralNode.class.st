Class {
	#name : #DartSetOrMapLiteralNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'elements',
		'typeArguments'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartSetOrMapLiteralNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitSetOrMapLiteral: self
]

{ #category : #generated }
DartSetOrMapLiteralNode >> elements [

	^ elements
]

{ #category : #generated }
DartSetOrMapLiteralNode >> elements: aDartElementsNode [

	self elements notNil ifTrue: [ self elements parent: nil ].
	elements := aDartElementsNode.
	self elements notNil ifTrue: [ self elements parent: self ]
]

{ #category : #generated }
DartSetOrMapLiteralNode >> nodeVariables [

	^ #( #elements #typeArguments )
]

{ #category : #generated }
DartSetOrMapLiteralNode >> typeArguments [

	^ typeArguments
]

{ #category : #generated }
DartSetOrMapLiteralNode >> typeArguments: aDartTypeListNode [

	self typeArguments notNil ifTrue: [ self typeArguments parent: nil ].
	typeArguments := aDartTypeListNode.
	self typeArguments notNil ifTrue: [ self typeArguments parent: self ]
]
