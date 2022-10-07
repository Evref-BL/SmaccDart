Class {
	#name : #DartFunctionTypeTailNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'parameterTypeList',
		'typeParameters'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartFunctionTypeTailNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitFunctionTypeTail: self
]

{ #category : #generated }
DartFunctionTypeTailNode >> nodeVariables [

	^ #( #typeParameters )
]

{ #category : #generated }
DartFunctionTypeTailNode >> otherVariables [

	^ #( #parameterTypeList )
]

{ #category : #generated }
DartFunctionTypeTailNode >> parameterTypeList [

	^ parameterTypeList
]

{ #category : #generated }
DartFunctionTypeTailNode >> parameterTypeList: anObject [

	self setParent: self parameterTypeList to: nil.
	parameterTypeList := anObject.
	self setParent: self parameterTypeList to: self
]

{ #category : #generated }
DartFunctionTypeTailNode >> typeParameters [

	^ typeParameters
]

{ #category : #generated }
DartFunctionTypeTailNode >> typeParameters: aDartTypeParametersNode [

	self typeParameters notNil ifTrue: [ self typeParameters parent: nil ].
	typeParameters := aDartTypeParametersNode.
	self typeParameters notNil ifTrue: [ 
		self typeParameters parent: self ]
]
