function writeFooter() {

	console.log(window.yamlData);
 
	// Get footer element
	const footer = document.getElementsByTagName("footer")[0];	
	
	// Add content
	footer.innerHTML = "<div>";
	footer.innerHTML += "© " + new Date().getFullYear() + " Me";
	footer.innerHTML += "</div>";
	footer.innerHTML += "<div>";
	footer.innerHTML += "Site designed by <a href=\"https://3danatomystudios.com/\" target=\"_blank\"> 3D Anatomy Studios</a> and maintained by Me";
	footer.innerHTML += "</div>";
	footer.innerHTML += "<div>";
	footer.innerHTML += "Settings";
	footer.innerHTML += "</div>";

	return;
}

writeFooter();