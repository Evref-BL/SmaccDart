Extension { #name : #SmaCCParseNode }

{ #category : #'*SmaCC_Spec_UI' }
SmaCCParseNode >> allNodes [

	| result |
	result := OrderedCollection new.
	self allNodesDo: [ :node | result add: node ].
	^ result
]

{ #category : #'*SmaCC_Spec_UI' }
SmaCCParseNode >> createCanvas: data [ 

	| canvas shapes |
	canvas := RSCanvas new.
	shapes := data collect: [ :entity | 
		          | box label |
		          box := RSBox new
			                 border: (RSBorder new color: Color black);
			                 noPaint.
		          label := RSLabel new
			                   text: entity class name;
			                   color: 'black';
			                   yourself.
		          box extent: label extent + 10.
		          { 
			          box.
			          label } asShape
			          model: entity;
			          draggable;
			          popup;
			          yourself ].
	canvas addAll: shapes.
	RSLineBuilder orthoVertical
		withVerticalAttachPoint;
		canvas: canvas;
		shapes: canvas nodes;
		connectToAll: #children.

	RSTreeLayout on: canvas nodes.
	canvas addInteraction: RSCanvasController.

	^ canvas
]

{ #category : #'*SmaCC_Spec_UI' }
SmaCCParseNode >> smaccSpecUIExtension [
	<inspectorPresentationOrder: 1 title: 'Graph'>
	^ SpRoassal3InspectorPresenter new
		  canvas: (self createCanvas: (self allNodes , {self}));
		  yourself
]

{ #category : #'*SmaCC_Spec_UI' }
SmaCCParseNode >> smaccSpecUISourceCode [

	<inspectorPresentationOrder: 50 title: 'Source'>
	^ SpCodePresenter new
		  lineNumbers: true;
		  syntaxHighlight: false;
		  text: self topParent completeSource asString;
		  selectionInterval: self sourceInterval;
		  yourself
]
