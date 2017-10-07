container_side_margin = 100
header_image_w = 750
header_image_h = 140
header_image_caption_bpadding = 22

sidebar_p = 0.25
bn_sidebar_main = 10
sidebar_padding = new Array(10, 0, 10, 20)
maincontent_padding = new Array(10, 10, 10, 10)

url_split = document.URL.split("/");

page_trail = new Array()
for(i = 1; i <= 4; i++){
	//if(url_split[url_split.length-i].toLowerCase() == "~aolsen" || url_split[url_split.length-i].toLowerCase() == "public_html") break
	if(url_split[url_split.length-i].toLowerCase() == "aaronolsen.github.io") break
	page_trail[i-1] = url_split[url_split.length-i].toLowerCase()
}

if(page_trail[0] == '') page_trail[0] = 'about_me';

// Get steps removed from file
if(url_split[4] == 'aaron'){
	if(url_split.length <= 6){url_level = 1}else{url_level = url_split.length - 9}
}else{
	if(url_split.length <= 4){url_level = 2}else{url_level = url_split.length - 2}
}
//alert(url_split + '; ' + url_split.length + '; ' + url_level)

window_size_adjust()

window.onresize = window_size_adjust

function window_size_adjust(){

	container_width = window.innerWidth - 2*container_side_margin
	sidebar_width = container_width*sidebar_p - sidebar_padding[1] - sidebar_padding[3]
	maincontent_width = container_width*(1-sidebar_p) - maincontent_padding[1] - maincontent_padding[3] - bn_sidebar_main - 4

	document.getElementById('container').style.width = (container_width) + 'px'

	document.getElementById('linksidebar').style.width = sidebar_width + 'px'
	document.getElementById('linksidebar').style.padding = sidebar_padding[0] + 'px ' + sidebar_padding[1] + 'px ' + sidebar_padding[2] + 'px ' + sidebar_padding[3] + 'px '
	document.getElementById('maincontent').style.width = maincontent_width + 'px'
	document.getElementById('maincontent').style.padding = maincontent_padding[0] + 'px ' + maincontent_padding[1] + 'px ' + maincontent_padding[2] + 'px ' + maincontent_padding[3] + 'px '

	document.getElementById('header_image').style.width = container_width + 'px'
	document.getElementById('header_image_caption').style.top = container_width*(1/(header_image_w/header_image_h)) - header_image_caption_bpadding + 'px';
	//alert((container_width*linksidebar_p) + (container_width*(1-linksidebar_p) - bn_sidebar_main))
}