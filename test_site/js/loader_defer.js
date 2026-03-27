// These are yaml files to be loaded
const yamlFilesToLoad = [
	'yaml/example.yaml',
];

// These are non local javascript files that will be loaded on all pages
// Files are loaded in the same order as listed
const localScriptFilesToLoad = [
	'footer.js',
];

// What to do once all of the internal scripts and window content are loaded
// Since the internal scripts are only loaded once the window is done loading (because of 
//	the defer property), this function should be called once all internal scripts are 
//	loaded and all window content is loaded
function onAllInternalScriptLoad() {

	showBodyContent();
};

var int_content_load_ct = 0;

// Load a particular yaml file
function loadYaml(src, ttl_count) {

	fetch(src)
		.then(response => response.text()) // Get the file content as text
		.then(yamlText => {
		  // Once the text is loaded, use the jsyaml library to parse it
			try {

				// Add data as a global variable so that any function can access it
				window.yamlData = jsyaml.load(yamlText);

				// Increase count of files that are loaded, if counting this file
				int_content_load_ct++;

				// Check if all content files are loaded
				// If yes, call function to run once all files are loaded
				if(int_content_load_ct == ttl_count){
					onAllContentLoad();
					int_content_load_ct = 0;
				}

				//const data = jsyaml.load(yamlText);
				//console.log("YAML data loaded successfully:", data);
				// You can now work with the 'data' object
				// e.g., console.log(data.user.name); 
			} catch (e) {
				console.error("Error parsing YAML:", e);
			}
		})
		.catch(error => {
			console.error("Error fetching the YAML file:", error);
		});
}

// What to do once all external scripts are loaded
function onAllExternalScriptLoad() {

	// Loop through the array and load each yaml file
	yamlFilesToLoad.forEach(src => {
		loadYaml(src, yamlFilesToLoad.length);
	});

	// Loop through the array and load each markdown file

}

// What to do once all content files are loaded
function onAllContentLoad() {

	// Loop through the array and load each script
	localScriptFilesToLoad.forEach(src => {
		loadScript('js/' + src, false, localScriptFilesToLoad.length);
	});
}

function showBodyContent() {

	// Delay the visibility of page content so that javascript changes to DOM have
	// time to take effect before rendering. This prevents flashes of incompletely 
	// styled content
	
	// It should not be necessary to use a delayDuration greater than 0 because this 
	// function should only be called once all content and scripts have been loaded
	
	// But if there is any persisting lag in loading the content, this is a last resort 
	// stop gap that can give the page additional time to load content before final display
	const delayDuration = 0;

	setTimeout(() => {
		// This code runs after the specified delay
		// Set display for the div with id 'content' to 'block' (default) to remove 'hidden' value
		const body = document.getElementsByTagName('body')[0];
		body.setAttribute('style', 'display:block !important');
	}, delayDuration);
}