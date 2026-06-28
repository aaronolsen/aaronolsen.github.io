let pubAuthorsBold = [];

function loadPubAuthorsBold() {

	console.log(window.yamlData['namestobold']);
	
	// Load the names 
	pubAuthorsBold = [
		'Losacco, Federica', 'Federica Losacco',
		'Lopes, Fernando', 'Fernando Lopes',
		'Rossini, Michele', 'Michele Rossini',
		'Tarasov, Sergei', 'Sergei I. Tarasov', 'Sergei Tarasov',
		'Porto, Diego', 'Diego Porto',
		'Montanaro, Giulio', 'Giulio Montanaro',
		'Merrien, Thomas', 'Thomas Merrien'
	];

}

loadPubAuthorsBold()