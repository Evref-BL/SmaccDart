"
A DartPreProcessTest is a test class for testing the behavior of DartPreProcess
"
Class {
	#name : #DartPreProcessTest,
	#superclass : #TestCase,
	#instVars : [
		'dpp'
	],
	#category : #'PreprocessingDart-Tests'
}

{ #category : #initialization }
DartPreProcessTest >> setUp [ 

	dpp := DartPreProcess new. 
	
]

{ #category : #test }
DartPreProcessTest >> testApplyRefactoringOnString [
	|code res|
	code := 'void main(List<String> args) {
  var hello = ''hello'';
  var s = '''''' this a 
  looooooo...
  oooooooo...
  oooooong
  way to say $hello'''''';
}'.

	res := dpp applyRefactoringOnString: code. 

	self assert:  res lines size equals: 4.
	
	
]
