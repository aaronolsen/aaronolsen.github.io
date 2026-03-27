// These are all the web-based javascript files that will be loaded on all pages
// Files will load ASAP (not deferred)
// Files are loaded in the same order as listed
const webScriptFilesToLoad = [
	'https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js',
	'https://cdnjs.cloudflare.com/ajax/libs/js-yaml/4.1.0/js-yaml.min.js',
];

var int_script_load_ct = 0;
var ext_script_load_ct = 0;

// Load a particular javascript file
function loadScript(src, external, ttl_count) {

	const script = document.createElement('script');
	script.src = src;

	// What to do once script is loaded
	script.onload = () => {

		// Increase count of files that are loaded, if counting this file
		if(external){
			ext_script_load_ct++;
		}else{
			int_script_load_ct++;
		}

		// Check if all counted scripts are loaded
		// If yes, call function to run once all files are loaded
		if(ext_script_load_ct == ttl_count){
			onAllExternalScriptLoad();
			ext_script_load_ct = 0;
		}
		if(int_script_load_ct == ttl_count){
			onAllInternalScriptLoad();
			int_script_load_ct = 0;
		}
	};

	script.async = false; // To ensure scripts execute in order if needed
	document.head.appendChild(script);
}

// Loop through the array and load each script
webScriptFilesToLoad.forEach(src => {
	loadScript(src, true, webScriptFilesToLoad.length);
});