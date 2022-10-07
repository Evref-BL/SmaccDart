Class {
	#name : #DartTypeNameNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeIdentifier',
		'followingTypeIdentifier'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeNameNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeName: self
]

{ #category : #generated }
DartTypeNameNode >> followingTypeIdentifier [

	^ followingTypeIdentifier
]

{ #category : #generated }
DartTypeNameNode >> followingTypeIdentifier: aSmaCCToken [

	followingTypeIdentifier := aSmaCCToken
]

{ #category : #generated }
DartTypeNameNode >> tokenVariables [

	^ #( #typeIdentifier #followingTypeIdentifier )
]

{ #category : #generated }
DartTypeNameNode >> typeIdentifier [

	^ typeIdentifier
]

{ #category : #generated }
DartTypeNameNode >> typeIdentifier: aSmaCCToken [

	typeIdentifier := aSmaCCToken
]
