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

fill_header()
fill_footer()
fill_linksidebar()
fill_title()
fill_pagetrail()
window_size_adjust()

window.onresize = window_size_adjust

function fill_pagetrail(){

	// CHECK LEVEL
	pre_add = ''
	for(k = 1;k < url_level; k++) pre_add += "../"
	
	//pre_add += '../../'

	t = '<a href="' + pre_add + 'about_me.html">home</a>'

	for(j=1, i = page_trail.length-1; i >= 0; i--, j++){

		t += "&nbsp;&nbsp;&nbsp;>&nbsp;&nbsp;&nbsp;"
		
		if(page_trail[i].lastIndexOf(".") > 0){
			page_name = page_trail[i].substring(0, page_trail[i].lastIndexOf("."))
		}else{
			page_name = page_trail[i]
		}
		
		parent_page = ''
		//alert(j)
		if(j == 2) parent_page = page_trail[i+1] + '/'
		if(j == 3) parent_page = page_trail[i+2] + '/' + page_trail[i+1] + '/'

		if(i > 0){
			t += "<a href=\"" + pre_add + parent_page + page_trail[i] + ".html\">" + page_name.replace("_", " ") + "</a>"
		}else{
			t += page_name.replace("_", " ")
		}
		
		j
	}

	document.getElementById('pagetrail').innerHTML = t
}

function fill_footer(){

	t = ''
	t += '<div style=\'float:left;\' >'
	t += '© ' + new Date().getFullYear() + ' Aaron Olsen. All rights reserved.<br><br>'
	t += '</div>'
	t += '<div style=\'float:right;\' >'
	t += 'Design and photographs by Aaron Olsen.'
	t += '</div>'
	t += '<div style=\'float:left;background-color:;\' >'
	t += 'This material is based upon work supported by the National Science Foundation (DGE-1144082, DGE-0903637, DBI-1612230). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.'
	t += '</div>'

	document.getElementById('footer').innerHTML = t
}

function fill_header(){

	//var image_idx = Math.round(Math.random()*(images.length-1));

	var img = new Array()
	img[img.length] = new Array('index.html', 'American Tree Sparrow', 'img/header_american_tree_sparrow.jpg', 'white')
	img[img.length] = new Array('about_me.html', 'American Tree Sparrow', 'img/header_american_tree_sparrow.jpg', 'white')
	img[img.length] = new Array('cv.html', 'Northern Shoveler, female', 'img/header_northern_shoveler_f.jpg', 'white')
	img[img.length] = new Array('software.html', 'Canada Goose', 'img/header_canada_goose.jpg', 'white')
	img[img.length] = new Array('current_projects.html', 'Great Horned Owl', 'img/header_great_horned_owl.jpg', 'white')
	//img[5] = new Array('cpp_code', 'Bald Eagle', 'img/header_bald_eagle.jpg', 'white')
	//img[5] = new Array('cpp_code', 'American Wigeon, male', 'img/header_american_wigeon_m.jpg', 'white')
	img[img.length] = new Array('about_me.html', 'Great Horned Owl', 'img/header_great_horned_owl.jpg', 'white')
	img[img.length] = new Array('contact.html', 'Red-tailed Hawk', 'img/header_redtailed_hawk.jpg', 'white')

	img[img.length] = new Array('stereomorph.html', 'Hairy Woodpecker', '../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('dietquery.html', 'Hairy Woodpecker', '../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('linkr.html', 'Hairy Woodpecker', '../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('svgviewr.html', 'Hairy Woodpecker', '../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('digitizing.html', 'Hairy Woodpecker', '../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('linkages.html', 'White Ibis', '../img/header_white_ibis.jpg', 'white')
	img[img.length] = new Array('waterfowl.html', 'Canada Goose', '../img/header_canada_goose.jpg', 'white')
	img[img.length] = new Array('suction.html', 'Purple Gallinule', '../img/header_purple_gallinule.jpg', 'white')

	img[img.length] = new Array('examples.html', 'Yellow-rumped Warbler', '../../img/headers/yellow_rumped_warbler.jpg', 'white')
	img[img.length] = new Array('interactive.html', 'Red-breasted Nuthatch', '../../img/headers/red_breasted_nuthatch.jpg', 'white')

	img[img.length] = new Array('3d_4-bar.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('owl.html', 'Red-breasted Nuthatch', '../../../img/headers/red_breasted_nuthatch.jpg', 'white')
	img[img.length] = new Array('salmon.html', 'Red-breasted Nuthatch', '../../../img/headers/red_breasted_nuthatch.jpg', 'white')
	img[img.length] = new Array('3d_5-bar.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('lur.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('4-bar_points.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('lssrssl.html', 'Hairy Woodpecker', '../../../img/headers/hairy_woodpecker.jpg', 'white')
	img[img.length] = new Array('rlss.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('rrss.html', 'Red-breasted Nuthatch', '../../../img/headers/red_breasted_nuthatch.jpg', 'white')
	img[img.length] = new Array('rsslssr.html', 'Red-tailed Hawk', '../../../img/header_redtailed_hawk.jpg', 'white')
	img[img.length] = new Array('rssrsspss.html', 'Dusky Blue Grouse', '../../../img/headers/dusky_blue_grouse.jpg', 'white')
	img[img.length] = new Array('sspssl.html', 'Canada Goose', '../../../img/header_canada_goose.jpg', 'white')
	
	// GET IMAGE INDEX BASED ON CURRENT PAGE
	img_idx = 0;
	for(i in img){
		if(page_trail[0] == img[i][0]){img_idx = i;break;}
	}
	
	t = ''
	//t += '<div class=\'header_title\' >'
	//t += 'Aaron Olsen'
	//t += '</div>'
	t += '<div style=\'position:relative;\' >'
	t += '<img id=\'header_image\' style=\'float:right;\' src=\'' + img[img_idx][2] + '\' />'
	t += '<div id=\'header_image_caption\' style=\'color:' + img[img_idx][3] + '\' class=\'header_image_caption\' >'
	t += img[img_idx][1]
	t += '</div>'
	t += '</div>'


	document.getElementById('header').innerHTML = t
}

function fill_linksidebar(){

	links = new Array()
	links[0] = new Array('About me', 'about_me.html')
	links[1] = new Array('Current projects', 'current_projects.html')
	links[1][2] = new Array('Suction feeding', 'suction.html')
	links[1][3] = new Array('Biomechanical linkages', 'linkages.html')
	links[1][4] = new Array('Waterfowl feeding', 'waterfowl.html')
	links[2] = new Array('Software', 'software.html')
	links[2][2] = new Array('StereoMorph', 'stereomorph.html')
	links[2][3] = new Array('Digitizing App', 'digitizing.html')
	links[2][4] = new Array('linkR', 'linkr.html')
	links[2][5] = new Array('svgViewR', 'svgviewr.html')
	links[2][6] = new Array('dietQuery', 'dietquery.html')
	//links[3] = new Array('C++ code', 'cpp_code.html')
	//links[3][2] = new Array('findCheckerboardCorners', 'find_checkerboard_corners.html')
	links[3] = new Array('Contact', 'contact.html')
	links[4] = new Array('CV', 'cv.html')

	t = ''
	for(i in links){

		// CHECK LEVEL
		pre_add = ''
		for(k = 1;k < url_level; k++) pre_add += "../"
	
		// WRITE MAIN SIDEBAR LINK
		t += '<div class=\'linksidebar_mainlink\' >'
			t += '<a href=\'' + pre_add + links[i][1] + '\'>'
				t += links[i][0]
			t += '</a>'
		t += '</div>'

		// RECORD IF ANY LINKS CORRESPOND TO CURRENT ADDRESS
		var sub_include = 0

		// CHECK MAIN ADDRESS FIRST
		if(page_trail[0] == links[i][1]) sub_include++
		
		// FOR THIRD LEVEL PAGES, CHECK GRANDPARENT PAGE
		if(url_level >= 3){
			if(page_trail[url_level-1] == links[i][1].substring(0, links[i][1].indexOf("."))) sub_include++
		}

		s = ''
		for(j = 2;j < links[i].length;j=j+1){

			// CHECK SUB ADDRESS
			if(page_trail[0] == links[i][j][1]) sub_include++

			// CHECK LEVEL
			pre_add = ''
			if(url_level == 1) pre_add = links[i][1].substring(0, links[i][1].indexOf(".")) + '/';
			if(url_level == 2) pre_add = '../' + links[i][1].substring(0, links[i][1].indexOf(".")) + '/';
			if(url_level == 3) pre_add = '../../' + links[i][1].substring(0, links[i][1].indexOf(".")) + '/';
			if(url_level == 4) pre_add = '../../../' + links[i][1].substring(0, links[i][1].indexOf(".")) + '/';

			// WRITE MAIN LINK
			s += '<div class=\'linksidebar_sublink\' >'
				s += '<a href=\'' + pre_add + links[i][j][1] + '\'>'
					//s += '&#9642; '
					s += links[i][j][0]
				s += '</a>'
			s += '</div>'
		}
		
		if(sub_include) t += s
	}

	document.getElementById('linksidebar').innerHTML = t
}

function fill_title(){
	
	var title='Aaron Olsen | '

	if(page_trail.length <= 2){
		if(page_trail[0] == '') title += 'About me'
		if(page_trail[0] == '.edu/~aolsen/') title += 'About me'
		if(page_trail[0] == '.edu/~aolsen') title += 'About me'
		if(page_trail[0] == 'index.html') title += 'About me'
		if(page_trail[0] == 'about_me.html') title += 'About me'
		if(page_trail[0] == 'current_projects.html') title += 'Current projects'
		if(page_trail[0] == 'software.html') title += 'Software'
		if(page_trail[0] == 'stereomorph.html') title += 'StereoMorph'
		if(page_trail[0] == 'digitizing.html') title += 'Digitizing App'
		if(page_trail[0] == 'linkages.html') title += 'Linkages'
		if(page_trail[0] == 'waterfowl.html') title += 'Waterfowl'
		if(page_trail[0] == 'linkr.html') title += 'linkR'
		if(page_trail[0] == 'svgviewr.html') title += 'svgViewR'
		if(page_trail[0] == 'dietquery.html') title += 'dietQuery'
		if(page_trail[0] == 'cpp_code.html') title += 'C++ code'
		if(page_trail[0] == 'contact.html') title += 'Contact'
		if(page_trail[0] == 'cv.html') title += 'CV'
	}else{
		if(page_trail[0] == 'examples.html' && page_trail[1] == 'linkr') title += 'linkR Examples'
		if(page_trail[0] == 'interactive.html') title += 'svgViewR Interactive'
		if(page_trail[0] == '3d_4-bar.html') title += 'linkR 3D 4-bar'
		if(page_trail[0] == '3d_5-bar.html') title += 'linkR 3D 5-bar'
		if(page_trail[0] == 'owl.html') title += 'linkR Owl'
		if(page_trail[0] == 'salmon.html') title += 'linkR Salmon'
		if(page_trail[0] == 'lssrssl.html') title += 'linkR LSSRSSL'
		if(page_trail[0] == 'rlss.html') title += 'linkR RLSS'
		if(page_trail[0] == 'rrss.html') title += 'linkR RRSS'
		if(page_trail[0] == 'rsslssr.html') title += 'linkR RSSLSSR'
		if(page_trail[0] == 'rssrsspss.html') title += 'linkR RSSRSSPSS'
		if(page_trail[0] == 'sspssl.html') title += 'linkR SSPSSL'
	}
	
	if(title == 'Aaron Olsen | ') title = 'Aaron Olsen | About me'
	
	document.title = title
}

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