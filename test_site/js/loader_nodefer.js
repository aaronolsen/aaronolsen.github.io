//
var debug = false;
var int_script_load_ct = 0;

// These are Markdown files to be loaded
const md_files_to_load = [
	'md/example.md',
];
var md_datafile_load_ct = 0;
var md_datafile_load_ttl = md_files_to_load.length;

// These are yaml files to be loaded
const yaml_files_to_load = [
	'yaml/example.yaml',
];
var yaml_datafile_load_ct = 0;
var yaml_datafile_load_ttl = yaml_files_to_load.length;

// These are bibtex files to be loaded
const bib_files_to_load = [
	'bib/example.bib',
];
var bib_datafile_load_ct = 0;
var bib_datafile_load_ttl = bib_files_to_load.length;


// Load a particular md file
function loadMdFile(src, total_md_files) {

	fetch(src)
		.then(response => response.text()) // Get the file content as text
		.then(markdownText => {

			// Once the text is loaded, use the Marked library to parse it
			try {

				// Parse the markdown as HTML
				marked_parsed = marked.parse(markdownText);
				
				// Sanitize the output HTML
				window.mdData = DOMPurify.sanitize(marked_parsed);
				
				// Increase count of markdown files that are loaded
				md_datafile_load_ct++;

				// Check if all data files are loaded
				isAllDataLoaded()

			} catch (e) {
				console.error("Error parsing Markdown:", e);
			}
		})
		.catch(error => {
			console.error("Error fetching the Markdown file:", error);
		});
}

// Function to load markdown parser and data
function loadMdParserAndData(scriptSrc, loadAsync, dataFiles) {

	const total_md_files = dataFiles.length;
	const script = document.createElement('script');
	script.src = scriptSrc;

	// What to do once script is loaded
	script.onload = () => {

		// Loop through the array and load each markdown file
		dataFiles.forEach(src => {
			loadMdFile(src, total_md_files);
		});

	};

	script.async = loadAsync;
	document.head.appendChild(script);
}

// Load Markdown parsing functions and Markdown data
loadMdParserAndData('js/marked_and_purify.js', true, md_files_to_load);

// Load a particular yaml file
function loadYaml(src) {

	fetch(src)
		.then(response => response.text()) // Get the file content as text
		.then(yamlText => {
		  // Once the text is loaded, use the jsyaml library to parse it
			try {

				// Add data as a global variable so that any function can access it
				window.yamlData = jsyaml.load(yamlText);

				// Increase count of markdown files that are loaded
				yaml_datafile_load_ct++;

				// Check if all data files are loaded
				isAllDataLoaded()

			} catch (e) {
				console.error("Error parsing YAML:", e);
			}
		})
		.catch(error => {
			console.error("Error fetching the YAML file:", error);
		});
}

// Function to load yaml parser and data
function loadYamlParserAndData(scriptSrc, loadAsync, dataFiles) {

	const script = document.createElement('script');
	script.src = scriptSrc;

	// What to do once script is loaded
	script.onload = () => {

		// Loop through the array and load each markdown file
		dataFiles.forEach(src => {
			loadYaml(src);
		});

	};

	script.async = loadAsync;
	document.head.appendChild(script);
}

// Load yaml parsing library and yaml data
loadYamlParserAndData('js/yaml.js', true, yaml_files_to_load);

function convertCharFromBibTeX(bibtexText) {
  if (!bibtexText) return '';

  let result = bibtexText;

  // 1. Map common BibTeX escaped patterns back to Unicode characters
  const bibtexMap = {
    '{\\\'a}': 'á', '{\\\'e}': 'é', '{\\\'i}': 'í', '{\\\'o}': 'ó', '{\\\'u}': 'ú',
    '{\\\'A}': 'Á', '{\\\'E}': 'É', '{\\\'I}': 'Í', '{\\\'O}': 'Ó', '{\\\'U}': 'Ú',
    '{\\\`a}': 'à', '{\\\`e}': 'è', '{\\\`i}': 'ì', '{\\\`o}': 'ò', '{\\\`u}': 'ù',
    '{\\\`A}': 'À', '{\\\`E}': 'È', '{\\\`I}': 'Ì', '{\\\`O}': 'Ò', '{\\\`U}': 'Ù',
    '{\\\"a}': 'ä', '{\\\"e}': 'ë', '{\\\"i}': 'ï', '{\\\"o}': 'ö', '{\\\"u}': 'ü',
    '{\\\"A}': 'Ä', '{\\\"E}': 'Ë', '{\\\"I}': 'Ï', '{\\\"O}': 'Ö', '{\\\"U}': 'Ü',
    '{\\\^a}': 'â', '{\\\^e}': 'ê', '{\\\^i}': 'î', '{\\\^o}': 'ô', '{\\\^u}': 'û',
    '{\\\^A}': 'Â', '{\\\^E}': 'Ê', '{\\\^I}': 'Î', '{\\\^O}': 'Ô', '{\\\^U}': 'Û',
    '{\\\~n}': 'ñ', '{\\\~o}': 'õ', '{\\\~a}': 'ã',
    '{\\\~N}': 'Ñ', '{\\\~O}': 'Õ', '{\\\~A}': 'Ã',
    '{\\\c{c}}': 'ç', '{\\\c{C}}': 'Ç', '{\\\ss}': 'ß',
    '{\\\o}': 'ø', '{\\\O}': 'Ø', '{\\\aa}': 'å', '{\\\AA}': 'Å'
  };

  // Replace macro patterns
  for (const [escape, unicode] of Object.entries(bibtexMap)) {
    result = result.replaceAll(escape, unicode);
  }

  // 2. Unescape structural syntax signs
  result = result
    .replace(/\\([&%$#_{}])/g, '$1')
    .replace(/\\textasciitilde\{\}/g, '~')
    .replace(/\\textasciicircum\{\}/g, '^')
    .replace(/\\textbackslash\{\}/g, '\\');

  responding  = "\href{https://doi.org/10.1126/science.aav3218}{Blasi et al. 2019}",
 
  result = result
  	.replace(/\\href{(.*?)}{(.*?)}/g, '<a href="$1" target="_blank">$2</a>')  // 
  	.replace(/&lt;(.*?)&gt;/g, '<$1>');  // Replace &lt;...&gt; with <...>

  return result;
}

function cleanString(str) {
	
	var return_str = str;
	
	// Remove white space from beginning and end
	return_str = return_str.trim();
	
	//console.log(return_str);
	
	// Remove , at the end of value strings
	return_str = return_str.replace(/,$/, '');
	
	// Remove curly brackets at beginning and end of string
	return_str = return_str.replace(/^[{"]|["}]$/g, '');

	//console.log('\t' + return_str);

	// Remove ", at the end of value strings
	return_str = return_str.replace(/",$/, '');

	// Remove extra \ that is sometimes present in bibtex entries
	return_str = return_str.replace(/\\'\\/g, "\\'");
	
	// Convert Bibtex characters
	return_str = convertCharFromBibTeX(return_str);
	
	return return_str;	
}

function parseBibTeX(bibText) {
    const entries = [];
    const lines = bibText.split(/\r?\n/);
    let currentEntry = null;
    let currentField = null;
    let currentValue = '';

    for (const line of lines) {
        const trimmed = line.trim();

        // Skip comments and empty lines
        if (!trimmed || trimmed.startsWith('%')) continue;

        // Start of a new entry (e.g., @article{key, ...})
        if (trimmed.startsWith('@')) {
            if (currentEntry) entries.push(currentEntry);
            const match = trimmed.match(/^@([a-zA-Z0-9]+)\s*\{\s*([^,\s]+)\s*,/);
            
            if (match) {
                currentEntry = {
                    type: match[1].toLowerCase(),
                    key: match[2],
                    fields: {}
                };
            }
            continue;
        }

        // Closing brace for an entry
        if (trimmed === '}') {
            if (currentEntry) {
                if (currentField && currentValue) {
                    currentEntry.fields[currentField] = cleanString(currentValue);
                }
                entries.push(currentEntry);
                currentEntry = null;
                currentField = null;
                currentValue = '';
            }
            continue;
        }

        // If an entry is active, parse its fields
        if (currentEntry) {
            // Field definition (e.g., author = {Name},)
            if (trimmed.includes('=')) {
                if (currentField && currentValue) {
                    currentEntry.fields[currentField] = cleanString(currentValue);
                }
                const parts = trimmed.split('=');
                currentField = parts[0].trim().toLowerCase();
                currentValue = parts.slice(1).join('=').trim();
            } else {
                // Continuation of a multi-line field
                currentValue += ' ' + trimmed;
            }
        }
    }

    return entries;
}

// Load a particular bibtex file
function loadBib(src) {

	fetch(src)
		.then(response => response.text()) // Get the file content as text
		.then(bibText => {

			// Once the text is loaded, use the citation library to parse it
			try {
			
				// Parse the bibtex
				window.bibData = parseBibTeX(bibText);
				
				// Increase count of markdown files that are loaded
				bib_datafile_load_ct++;

				// Check if all data files are loaded
				isAllDataLoaded()

			} catch (e) {
				console.error("Error parsing Bibtex:", e);
			}
		})
		.catch(error => {
			console.error("Error fetching the Bibtex file:", error);
		});
}

// Loop through the array and load each markdown file
bib_files_to_load.forEach(src => {
	loadBib(src);
});

// These are non local javascript files that will be loaded on all pages
// Files are loaded in the same order as listed
const localScriptFilesToLoad = [
	'content.js'
];

if(document.title == 'Publications') localScriptFilesToLoad.push('publications.js');

// What to do once all of the internal scripts and window content are loaded
// Since the internal scripts are only loaded once the window is done loading (because of 
//	the defer property), this function should be called once all internal scripts are 
//	loaded and all window content is loaded
function onAllInternalScriptLoad() {

	showBodyContent();
};

// Load a particular javascript file
function loadScript(src, loadAsync, external, ttl_count) {

	const script = document.createElement('script');
	script.src = src;
	script.async = loadAsync;

	// What to do once script is loaded
	script.onload = () => {

		// Increase count of files that are loaded, if counting this file
		if(external){
		}else{
			int_script_load_ct++;
		}

		// Check if all counted scripts are loaded
		if(int_script_load_ct == ttl_count){
			onAllInternalScriptLoad();
			int_script_load_ct = 0;
		}
	};
	script.onerror = function() {
		console.error(`Failed to load the script: ${this.src}`);
	};

	document.head.appendChild(script);
}

// What to do once all content files are loaded
function isAllDataLoaded() {

	if(debug) console.log('isAllDataLoaded() called');
	if(debug) console.log('\t' + md_datafile_load_ct + ' of ' + md_datafile_load_ttl + ' markdown file(s) loaded');
	if(debug) console.log('\t' + yaml_datafile_load_ct + ' of ' + yaml_datafile_load_ttl + ' yaml file(s) loaded');
	if(debug) console.log('\t' + bib_datafile_load_ct + ' of ' + bib_datafile_load_ttl + ' bibtex file(s) loaded');

	if(md_datafile_load_ct < md_datafile_load_ttl) return;
	if(yaml_datafile_load_ct < yaml_datafile_load_ttl) return;
	if(bib_datafile_load_ct < bib_datafile_load_ttl) return;

	if(debug) console.log('All data files are loaded');

	// Loop through the array and load each script
	localScriptFilesToLoad.forEach(src => {
		loadScript('js/' + src, true, false, localScriptFilesToLoad.length);
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