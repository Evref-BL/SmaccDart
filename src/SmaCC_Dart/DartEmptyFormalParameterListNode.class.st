Class {
	#name : #DartEmptyFormalParameterListNode,
	#superclass : #DartProgramNode,
	#category : #'SmaCC_Dart'
}

{ #category : #generated }
DartEmptyFormalParameterListNode >> acceptVisitor: aProgramVisitor [

	^ aProgramVisitor visitEmptyFormalParameterList: self
]
