library(tiff)
library(jpeg)

create_main_page <- function(example_tiff_fdir, example_names){

	web_anim_fdir <- '/Users/aaron/Documents/Research/github/aaronolsen.github.io/software/linkr/animations/'
	example_html_fdir <- '/Users/aaron/Documents/Research/R package Tutorials/linkR/Modeling linkages in linkR v1.1/R/examples/'
	example_jpg_fdir <- '/Users/aaron/Documents/Research/github/aaronolsen.github.io/img/linkr/'
	example_r_fdir <- '/Users/aaron/Documents/Research/R dev/linkR/inst/extdata/test_configs/'

	html_lines <- '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	<title></title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" >

	<link rel="shortcut icon" href="http://www.uchicago.edu/favicon.ico"/>
	<link href="../../css/stylesheet.css" rel="stylesheet" type="text/css">

	<body>
		<div id="container" class="container">
			<div id="header" class="header" ></div>
			<div id="linksidebar" class="linksidebar" ></div>
			<div id="maincontent" class="maincontent" >
				<div id="pagetrail" class="pagetrail" ></div>
				<h1>Example Gallery for linkR</h1>

				<p>
				Below are examples of different levers and linkage mechanisms 
				modeled using <strong>linkR</strong>. Click on an example to view an 
				interactive animation of the linkage or click on "Code" to view the R code used
				to create the model. See
				<a href="../svgviewr/interactive.html" >svgViewR Interactive Commands</a>
				for instructions on how to interact with svgViewR animations.
				</p>
				'

	# Get examples in folder
	example_r_files <- list.files(example_r_fdir)
	example_tiff_files <- list.files(example_tiff_fdir)

	for(ii in 1:length(example_tiff_files)){
	
		jpg_fname <- gsub('[.]tiff', '.jpg', tolower(example_tiff_files[ii]))
	
		# Find jpg image filepath
		jpg_fpath <- paste0(example_jpg_fdir, jpg_fname)
		
		# Check if file exists, if not, convert from tiff
		if(!file.exists(jpg_fpath)){

			# Find tiff image filepath
			tiff_fpath <- paste0(example_tiff_fdir, example_tiff_files[ii])
			
			# Convert
			img <- readTIFF(tiff_fpath, native=TRUE)
			writeJPEG(img, target = jpg_fpath, quality = 1)
		}
		
		# Get image dimensions
		img <- readJPEG(jpg_fpath, native=TRUE)
		width <- dim(img)[2]
		height <- dim(img)[1]
		
		set_width <- 170
		set_height <- 130
		dim_scale <- (set_width/width)
		style <- paste0('width:', set_width, 'px;')
		if(dim_scale*height > set_height){
			dim_scale <- set_height / height
			style <- paste0('height:', set_height, 'px;')
		}
		
		style <- paste0(style, 'margin-top:', round(130 - dim_scale*height), 'px;')
		
		# Find html animation filepath
		html_anim_fname <- gsub('[.]jpg', '.html', jpg_fname)
		
		# Check if animation file already exists, if not copy over
		if(!file.exists(paste0(web_anim_fdir, html_anim_fname))){
			web_anim_fpath <- paste0(web_anim_fdir, html_anim_fname)
			html_anim_fpath <- paste0(example_html_fdir, html_anim_fname)
			if(!file.exists(html_anim_fpath)) stop(paste0("File '", html_anim_fpath, "' not found"))
			file.copy(html_anim_fpath, web_anim_fpath)
		}
		
		# Div block
		new_lines <- paste0('
				<div class="examplegallery" style="position:relative;">
					<a href="animations/', html_anim_fname, '" target="_blank" >
						<img class="examplegalleryimg" style="', style, '" src="../../img/linkr/', jpg_fname, '" >
					</a>
					<div class="examplegallerytext" style="position:absolute;bottom:5px;width:180px;" >
						', example_names[[ii]][2], '<br />
						<a href="examples/', html_anim_fname, '" >Code</a>
					</div>
				</div>
		')

		html_lines <- c(html_lines, new_lines)
	}

	html_lines <- c(html_lines, '\t\t\t</div>
			<div id="footer" class="footer" ></div>
		</div>
	</body>

<script src="../../js/sharedfunctions.js" type="text/javascript" ></script>')

	writeLines(html_lines, con='examples.html')
}

create_code_html_pages <- function(example_tiff_fdir, example_names){

	example_tiff_files <- list.files(example_tiff_fdir)
	test_config_fdir <- '/Users/aaron/Documents/Research/R dev/linkR/inst/extdata/test_configs/'
	html_code_fdir <- '/Users/aaron/Documents/Research/github/aaronolsen.github.io/software/linkr/examples/'

	for(ii in 1:length(example_tiff_files)){

		html_anim_fname <- gsub('[.]tiff', '.R', tolower(example_tiff_files[ii]))
		
	html_lines_pre <- paste0('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<title></title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" >

<link rel="shortcut icon" href="http://www.uchicago.edu/favicon.ico"/>
<link href="../../../css/stylesheet.css" rel="stylesheet" type="text/css">

<body>
	<div id="container" class="container">
		<div id="header" class="header" ></div>
		<div id="linksidebar" class="linksidebar" ></div>
		<div id="maincontent" class="maincontent" >
			<div id="pagetrail" class="pagetrail" ></div>
			<h1>', example_names[[gsub('[.]tiff', '', tolower(example_tiff_files[ii]))]][2], '</h1>
			
			<p>
			To run the example below, ensure that you have 
			<a href="http://rweb.quant.ku.edu/cran/" target="_blank" >R</a>
			installed on your system as well as the R packages 
			<a href="http://cran.r-project.org/web/packages/linkR/index.html" target="_blank" >linkR</a>
			and 
			<a href="http://cran.r-project.org/web/packages/svgViewR/index.html" target="_blank" >svgViewR</a>.
			Then, copy and paste the following code into the R console.
			</p>

			<div class="code" >
				<pre class="code">')

	html_lines_post <- paste0('</pre>
			</div>
			<p>
			This will create an 
			<a href="../animations/', gsub('[.]tiff', '', tolower(example_tiff_files[ii])), '.html" >interactive visualization</a> as an ".html" file that can be opened in any 
			compatible web browser. See
			<a href="../../svgviewr/interactive.shtml" >
			svgViewR Interactive Commands
			</a>
			for instructions on how to interact with svgViewR animations.
			</p>
			<br /><br />

		</div>
		<div id="footer" class="footer" ></div>
	</div>
</body>

<script src="../../../js/sharedfunctions.js" type="text/javascript" ></script>')

		test_config_fpath <- paste0(test_config_fdir, html_anim_fname)
		
		if(!file.exists(test_config_fpath)) stop(paste0("File '", test_config_fpath, "' not found"))
		
		web_code_fpath <- paste0(html_code_fdir, gsub('[.]r', '.html', html_anim_fname, ignore.case=TRUE))
		
		#if(!file.exists(web_code_fpath)){

			write_lines <- paste0(html_lines_pre, paste(suppressWarnings(readLines(test_config_fpath)), collapse='\n'), html_lines_post)
		
			writeLines(write_lines, con=web_code_fpath)
		#}

		#break
	}


}

example_tiff_fdir <- '/Users/aaron/Documents/Research/R package Tutorials/linkR/Modeling linkages in linkR v1.1/tiff/examples/'

# Set example print names
example_names <- list(
	c('lssl', 'Two coupled sliders'),
	c('lsslssl', 'Three coupled sliders'),
	c('lsslsspss', 'Coupled linear, planar sliders'),
	c('lssrssl', 'Sliders coupled by rotating link'),
	c('owl', 'Bird cranial linkage'),
	c('r(ssl)ssr', 'Slider, 2D 4-bar in series'),
	c('rlss', 'Slider along rotating link'),
	c('rrrsspsssspssss', 'Fish cranial linkage configuration'),
	c('rrss', 'Rotating links in series'),
	c('rs(ssl)sr', 'Slider in parallel with 4-bar'),
	c('rssl', 'Crank-driving sliding link'),
	c('rsslssr', 'Coupled rotating, linear links'),
	c('rssr_3d', '3D 4-bar'),
	c('rssr', 'Planar 4-bar'),
	c('rssr(ssl)', 'Slider, 3D 4-bar in series'),
	c('rssrsspss', 'Coupled rotating, planar links'),
	c('rssrssr', 'Two 3D 4-bars in series'),
	c('salmon', 'Fish cranial linkage'),
	c('sspssl', 'Coupled planar and linear sliders')
)

# Set first entry as name
for(ii in 1:length(example_names)) names(example_names)[ii] <- example_names[[ii]][1]

create_main_page(example_tiff_fdir, example_names)
create_code_html_pages(example_tiff_fdir, example_names)