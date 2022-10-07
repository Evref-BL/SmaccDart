Class {
	#name : #DartTypeNotVoidNotFunctionListNode,
	#superclass : #DartProgramNode,
	#instVars : [
		'typeNotVoidNotFunction',
		'nextTypeNotVoidNotFunctions'
	],
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitTypeNotVoidNotFunctionList: self
]

{ #category : #'generated-initialize-release' }
DartTypeNotVoidNotFunctionListNode >> initialize [
	super initialize.
	nextTypeNotVoidNotFunctions := OrderedCollection new: 2.
]

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> nextTypeNotVoidNotFunctions [

	^ nextTypeNotVoidNotFunctions
]

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> nextTypeNotVoidNotFunctions: anOrderedCollection [

	self setParent: self nextTypeNotVoidNotFunctions to: nil.
	nextTypeNotVoidNotFunctions := anOrderedCollection.
	self setParent: self nextTypeNotVoidNotFunctions to: self
]

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> otherVariables [

	^ #( #typeNotVoidNotFunction #nextTypeNotVoidNotFunctions )
]

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> typeNotVoidNotFunction [

	^ typeNotVoidNotFunction
]

{ #category : #generated }
DartTypeNotVoidNotFunctionListNode >> typeNotVoidNotFunction: anObject [

	self setParent: self typeNotVoidNotFunction to: nil.
	typeNotVoidNotFunction := anObject.
	self setParent: self typeNotVoidNotFunction to: self
]
