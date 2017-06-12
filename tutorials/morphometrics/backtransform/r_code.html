#library(StereoMorph)
#library(svgViewR)
#library(rgl)

source('/Users/aaron/Documents/Research/R Package Tests/StereoMorph/load dependent packages.R')

R_dir <- "/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/R"
source_apply <- sapply(paste0(R_dir, "/", list.files(paste0(R_dir, ""))), source, .GlobalEnv)

R_dir <- "/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/svgViewR/R"
source_apply <- sapply(paste0(R_dir, "/", list.files(paste0(R_dir, ""))), source, .GlobalEnv)

measure <- FALSE
calibrate <- FALSE
test <- FALSE
digitize <- TRUE
reconstruct <- FALSE
reflect <- FALSE
align_shapes <- FALSE
draw <- FALSE
read_shapes <- FALSE

if(read_shapes){
	
	# Read shapes to list
	#read_shapes <- readShapes('Run 1/Reconstruct/Shapes 3D')
	file <- c('Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt', 
	'Run 1/Reconstruct/Shapes 3D/psittacus_erithacus_FMNH312899_a1.txt')
	read_shapes <- readShapes(file=c('Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt', 'Run 1/Reconstruct/Shapes 3D/psittacus_erithacus_FMNH312899_a1.txt'))
}

if(FALSE){
	library(scatterplot3d)

	library(StereoMorph)
	library(rgl)
	library(car)

	file <- 'Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt'
	shapes <- readShapes(file=file)
	lm <- shapes$landmarks
	r <- apply(lm, 2, 'max') - apply(lm, 2, 'min')
	plot3d(lm, aspect=c(r/r[3]), size=7)
	lapply(shapes$curves, plot3d, size=4, col="lightblue", add=TRUE)
	rgl.postscript(filename='bubo_virginianus_FMNH488595_reconstruct.eps')
	rgl.postscript(filename='bubo_virginianus_FMNH488595_aligned.eps')


	rgl.snapshot(filename='bubo_virginianus_FMNH488595_reconstruct.png')
	rgl.points(lm)
	lapply(read_shapes$curves, rgl.points, color="lightblue", add=TRUE)
	axes3d()

	scatterplot3d(lm[, 1], lm[, 2], lm[, 3])
	scatter3d(lm[,1], lm[,2], lm[,3], sphere.size=0.5, surface=FALSE)
}

if(reconstruct){

	# Reconstruct landmarks (and curves, if present)
	rss <- reconstructStereoSets(shapes.2d='Run 1/Reconstruct/Shapes 2D', 
		shapes.3d='Run 1/Reconstruct/Shapes 3D', 
		cal.file='Run 1/Calibrate/calibration.txt', 
		set.names='psittacus_erithacus_FMNH312899', 
		#update.only=FALSE, 
		reconstruct.curves=TRUE,
		#unify=FALSE,
		min.fill.tangency=10, 
		verbose=FALSE,
		even.spacing='Shapes ref/even_spacing.txt', 
		min.common=5 
		#even.spacing=100, 
		)
}

if(reflect){

	# Reflect missing landmarks and curve points
	rms <- reflectMissingShapes(shapes='Run 1/Reconstruct/Shapes 3D', 
		file='Run 1/Reconstruct/Shapes 3D reflected', average=TRUE)

	# Single shapes file input
	#specimen <- 'bubo_virginianus_FMNH488595'
	#shapes <- paste0('Run 1/Reconstruct/Shapes 3D/', specimen, '.txt')
	#file <- paste0('Run 1/Reconstruct/Shapes 3D reflected/', specimen, '.txt')
	#reflectMissingShapes(shapes=shapes, file=file, average=TRUE)

	# Vector of shapes files input, vector of shapes files output
	#shapes <- c('Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt', 'Run 1/Reconstruct/Shapes 3D/psittacus_erithacus_FMNH312899_a1.txt')
	#file <- c('Run 1/Reconstruct/Shapes 3D reflected/bubo_virginianus_FMNH488595.txt', 'Run 1/Reconstruct/Shapes 3D reflected/psittacus_erithacus_FMNH312899_a1.txt')
	#reflectMissingShapes(shapes=shapes, file=file, average=TRUE)

	# Shapes list input, file output
	#shapes.file <- 'Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt'
	#shapes <- readShapes(file=shapes.file)
	#file <- 'Run 1/Reconstruct/Shapes 3D reflected/bubo_virginianus_FMNH488595.txt'
	#reflectMissingShapes(shapes=shapes, file=file, average=TRUE)

	# Shapes list input, NULL output
	#shapes.file <- 'Run 1/Reconstruct/Shapes 3D/bubo_virginianus_FMNH488595.txt'
	#shapes <- readShapes(file=shapes.file)
	#shapes_rms <- reflectMissingShapes(shapes=shapes.file, file=NULL, average=TRUE)
}

if(align_shapes){

	# Reflect missing landmarks and curve points
	asm <- alignShapesToMidline(shapes='Run 1/Reconstruct/Shapes 3D reflected', 
		file='Run 1/Reconstruct/Shapes 3D aligned', print.progress=TRUE)

	# Single shapes file input
	#specimen <- 'bubo_virginianus_FMNH488595'
	#shapes <- paste0('Run 1/Reconstruct/Shapes 3D reflected/', specimen, '.txt')
	#file <- paste0('Run 1/Reconstruct/Shapes 3D aligned/', specimen, '.txt')
	#asm <- alignShapesToMidline(shapes=shapes, file=file)

	# Vector of shapes files input, vector of shapes files output
	#shapes <- c('Run 1/Reconstruct/Shapes 3D reflected/bubo_virginianus_FMNH488595.txt', 'Run 1/Reconstruct/Shapes 3D reflected/psittacus_erithacus_FMNH312899_a1.txt')
	#file <- c('Run 1/Reconstruct/Shapes 3D aligned/bubo_virginianus_FMNH488595.txt', 'Run 1/Reconstruct/Shapes 3D aligned/psittacus_erithacus_FMNH312899_a1.txt')
	#alignShapesToMidline(shapes=shapes, file=file)

	# Shapes list input, file output
	#shapes.file <- 'Run 1/Reconstruct/Shapes 3D reflected/bubo_virginianus_FMNH488595.txt'
	#shapes <- readShapes(file=shapes.file)
	#file <- 'Run 1/Reconstruct/Shapes 3D aligned/bubo_virginianus_FMNH488595.txt'
	#alignShapesToMidline(shapes=shapes, file=file)

	# Shapes list input, NULL output
	#shapes.file <- 'Run 1/Reconstruct/Shapes 3D reflected/bubo_virginianus_FMNH488595.txt'
	#shapes <- readShapes(file=shapes.file)
	#shapes_asm <- alignShapesToMidline(shapes=shapes.file, file=NULL)
}


if(draw){

	# Draw shapes
	for(file in list.files('Run 1/Reconstruct/Shapes 3D')){
		drawShapes(shapes=paste0('Run 1/Reconstruct/Shapes 3D/', file), 
			file=paste0('Run 1/Reconstruct/View shapes/', gsub('.txt', '', file), '.html'), 
			path.connect='Shapes ref/path_connect.txt',
			fdir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/svgViewR/inst/extdata/')
	}

	for(file in list.files('Run 1/Reconstruct/Shapes 3D aligned')){
		drawShapes(shapes=paste0('Run 1/Reconstruct/Shapes 3D aligned/', file), 
			file=paste0('Run 1/Reconstruct/View shapes/', gsub('.txt', '', file), ' aligned.html'), 
			path.connect='Shapes ref/path_connect.txt',
			fdir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/svgViewR/inst/extdata/')
	}

	for(file in list.files('Run 1/Reconstruct/Shapes 3D reflected')){
		drawShapes(shapes=paste0('Run 1/Reconstruct/Shapes 3D reflected/', file), 
			file=paste0('Run 1/Reconstruct/View shapes/', gsub('.txt', '', file), ' reflected.html'), 
			path.connect='Shapes ref/path_connect.txt',
			fdir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/svgViewR/inst/extdata/')
	}
}

if(measure){

	# Measure checkerboard square size
	digitizeImages(image.file='Measure square size/Images', 
		shapes.file='Measure square size/Shapes'
		#,app.dir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImages'
		)
}

if(calibrate){

	# Calibrate cameras
	cal_cam <- calibrateCameras(img.dir='Run 1/Calibrate/Images', cal.file='Run 1/Calibrate/calibration.txt', 
		corner.dir='Run 1/Calibrate/Corners', sq.size='6.35 mm', nx=8, ny=6,
		verify.dir='Run 1/Calibrate/Images verify')
}

if(test){

	# Test calibration
	test_cal <- testCalibration(img.dir='Run 1/Test calibration/Images', 
		cal.file='Run 1/Calibrate/calibration.txt', 
		corner.dir='Run 1/Test calibration/Corners', sq.size='5.080 mm', nx=7, ny=6,
		plot.dir='Run 1/Test calibration/Error tests', 
		verify.dir='Run 1/Test calibration/Images verify')
}

if(digitize){

	# Digitize photographs
	digitizeImages(
		image.file='Run 1/Reconstruct/Images', 
		shapes.file='Run 1/Reconstruct/Shapes 2D', 
		landmarks.ref = 'Shapes ref/landmarks_ref.txt',
		curves.ref = 'Shapes ref/curves_ref.txt',
		cal.file='Run 1/Calibrate/calibration.txt',
		,app.dir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImages'
	)
}