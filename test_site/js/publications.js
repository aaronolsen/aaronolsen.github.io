let pubAuthorsBold = window.yamlData['namestobold'];

function bibtex2html(bibtex) {

	let entry = "";
    let entries = "";
    let vol_num_pgs = "";
    let doi_clean = "";

    for (const bibtex_entry of bibtex) {
    
		// Create Entry
	    entry = "";

		if(bibtex_entry['fields']['author'] !== undefined){
			entry += formatBibtexAuthors(bibtex_entry['fields']['author']) + ' ';
		}

		if(bibtex_entry['fields']['year'] !== undefined){
			entry += bibtex_entry['fields']['year'] + '. ';
		}

		if(bibtex_entry['fields']['title'] !== undefined){
			entry += bibtex_entry['fields']['title'] + '. ';
		}

		if(bibtex_entry['fields']['journal'] !== undefined){
			entry += '<em>' + bibtex_entry['fields']['journal'] + '</em>';
		}

		vol_num_pgs = "";
		if(bibtex_entry['fields']['volume'] !== undefined && bibtex_entry['fields']['volume'] !== ""){
			vol_num_pgs += bibtex_entry['fields']['volume'];
		}
		if(bibtex_entry['fields']['number'] !== undefined && bibtex_entry['fields']['number'] !== ""){
			vol_num_pgs += '(' + bibtex_entry['fields']['number'] + ')';
		}
		if(bibtex_entry['fields']['pages'] !== undefined && bibtex_entry['fields']['pages'] !== ""){
			if(bibtex_entry['fields']['pages'].includes('-')){
				vol_num_pgs += ', pp.' + bibtex_entry['fields']['pages'].replace(/--/, '-');
			}else{
				vol_num_pgs += ', p.' + bibtex_entry['fields']['pages'];
			}
		}
		if(vol_num_pgs == ""){
			entry += '.';
		}else{
			entry += ', ' + vol_num_pgs + '.';
		}
		
		if(bibtex_entry['fields']['predoi'] !== undefined){
			entry += ' ' + (bibtex_entry['fields']['predoi'] + '.').trim().replace('..', '.');
		}

		if(bibtex_entry['fields']['doi'] !== undefined){
			doi_clean = bibtex_entry['fields']['doi'].replace('https://doi.org/', '');
			entry += ' DOI: <a href="https://doi.org/' + doi_clean + '" target="_blank" >' + doi_clean + '</a>.';
		}else{
			if(bibtex_entry['fields']['url'] !== undefined){
				entry += ' URL: <a href="' + bibtex_entry['fields']['url'] + '" target="_blank" >' + bibtex_entry['fields']['url'] + '</a>.';
			}
		}

		if(bibtex_entry['fields']['git'] !== undefined){
			entry += ' Github: <a href="' + bibtex_entry['fields']['git'] + '" target="_blank" >' + bibtex_entry['fields']['git'] + '</a>.';
		}

		if(bibtex_entry['fields']['pdf'] !== undefined){
			entry += ' PDF: <a href="' + bibtex_entry['fields']['pdf'] + '" target="_blank" >' + bibtex_entry['fields']['pdf'] + '</a>.';
		}

		// Add italic tags to entry
		entry = entry.replace(/\\textit\{(.*?)\}/g, '<em>$1</em>');

		// Replace any "as is" text
		entry = entry.replace(/\{(.*?)\}/g, '$1');

		// Add entry to entries
		entries += "<p>" + entry + "</p>";
	}

	return entries;
}

function formatBibtexAuthors(authors) {
	
	let author_formatted = "";
	let authors_formatted = "";
	let author_split = [];
	let author_lastname = "";
	let author_firstnames = [];
	let author_firstletters = [];

	// Split authors list into individual authors
	let authors_split = authors.split(" and ");
	
	let i = 0;
	
    for (const author of authors_split) {

    	// If string starts with {, preserve string as is
    	if(author[0] == "{"){

			author_formatted = author.replace(/^\{|\}$/g, '');
			
    	}else{

			// Split author name at delimiter
			if(author.includes(',')){

				// Split at , -- Names are Last name first
				author_split = author.split(",");

				// Trim whitespaces at ends of each entry
				author_split = author_split.map(item => item.trim());
			
				// Set author last name
				author_lastname = author_split[0];

				// Split first names
				author_firstnames = author_split.slice(1)[0].split(" ");

				// Trim whitespaces at ends of each entry
				author_firstnames = author_firstnames.map(item => item.trim());

			}else{

				// Split at whitespace -- Names are First name first
				author_split = author.split(" ");

				// Trim whitespaces at ends of each entry
				author_split = author_split.map(item => item.trim());

				// Set author last name
				author_lastname = author_split.at(-1);

				// Remove periods at end of any elements
				//author_split = author_split.map(item => item.replace(/.$/, ''));

				// Split first names
				author_firstnames = author_split.slice(0, -1);
			}

			// Get first letters of first names
			author_firstletters = author_firstnames.map(word => word[0]);

			// Last name and first letter of other names
			author_formatted = author_lastname + ', ' + author_firstletters.join('.') + '.';
    	}

    	// Check if author should be bolded
    	if(pubAuthorsBold.includes(author)){
    		author_formatted = "<strong>" + author_formatted + "</strong>";
    	}
    	
    	// Add author to string
    	authors_formatted += author_formatted;

		// Add delimiter between authors
		if(i < authors_split.length-2){
			authors_formatted += ", ";
		}else if(i == authors_split.length-2){
			authors_formatted += " & ";
		}
    	
    	i++;
	}
	
	return authors_formatted;
}