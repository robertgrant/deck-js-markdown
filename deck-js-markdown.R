# Function to convert my own markdown to deck.js slides

# To do:
#	allow local and global URLs for CSS & JS
#	add 'a' tag for audio and 'v' for video
#	arguments to turn on/off goto controls
#	hyperlinks for HTML-dodgers


makedeck<-function(txtfile,htmlfile,title=NA,author=NA,style="swiss") {
fin<-file(txtfile)
txt<-readLines(fin)
close(fin)

slides<-c("","<!-- The slides follow -->")
in.slide<-in.ul<-in.ol<-in.wl<-0
# At present, in.slide does not get used, but it might come in handy later

for (i in 1:length(txt)) {
	ch<-substr(txt[i],1,1)
	
	if(i==1) { # first slide
		in.slide<-1
		slides<-c(slides,
			"<section class=\"slide\">")
	}

	if(ch=="t") { # specified title
		in.slide<-1
		mytitle<-substring(txt[i],3)
		slides<-c(slides,
			paste("<h1>",mytitle,"</h1>",sep=""))
	}
	if(ch=="a") { # specified "author" line
		in.slide<-1
		myauthor<-substring(txt[i],3)
		slides<-c(slides,
			paste("<h3><em>",myauthor,"</em></h3>",sep=""))
	}

	if((ch=="-") & (i>1) & (i<length(txt))) { # close old slide and start a new one
		in.slide<-1
		if(in.ol==1) { # this assumes in.ol & in.ul can't both be 1...
			slides<-c(slides,
				"</ol>",
				"</section>",
				"",
				"<section class=\"slide\">")
		}
		if(in.ul==1) { # this assumes in.ol & in.ul can't both be 1...
			slides<-c(slides,
				"</ul>",
				"</section>",
				"",
				"<section class=\"slide\">")
		}
		if(in.wl==1) { 
			slides<-c(slides,
				"</ul>","</li>",
				"</ul>",
				"</section>",
				"",
				"<section class=\"slide\">")
		}
		if(in.ol==0 & in.ul==0 & in.wl==0) {
			slides<-c(slides,
				"</section>",
				"",
				"<section class=\"slide\">")
		}
		in.ul<-in.ol<-in.wl<-0
	}
	if((ch=="-") & (i==length(txt))) { # last slide
		in.slide<-1
		if(in.ol==1) { # this assumes in.ol & in.ul can't both be 1...
			slides<-c(slides,
				"</ol>",
				"</section>")
		}
		if(in.ul==1) { # this assumes in.ol & in.ul can't both be 1...
			slides<-c(slides,
				"</ul>",
				"</section>")
		}
		if(in.wl==1) { 
			slides<-c(slides,
				"</ul>","</li>",
				"</ul>",
				"</section>")
		}
		else {
			slides<-c(slides,
				"</section>")
		}
		in.ul<-in.ol<-in.wl<-0
	}

	if(ch=="h") {
		in.slide<-1
		slides<-c(slides,
			paste("<h2>",substring(txt[i],3),"</h2>",sep=""))
		in.ul<-in.ol<-in.wl<-0
	}

	if(ch=="u") {
		if((in.ul==0) & (in.ol==0) & (in.wl==0)) {
			slides<-c(slides,"<ul>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.ol==1) {
			slides<-c(slides,"</ol>","<ul>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.ul==1) {
			slides<-c(slides,
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.wl==1) {
			slides<-c(slides,"</ul>","</li>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		in.ul<-in.slide<-1
		in.ol<-in.wl<-0
	}

	if(ch=="w") { # double-u l geddit?
		if(in.ol==1) {
			slides<-c(slides,"</ol>","<ul>","<li>","<ul>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.ul==1) {
			slides<-c(slides,paste("<ul>",
							"<li>",
							substring(txt[i],3),
							"</li>",sep=""))
		}
		if(in.wl==1) {
			slides<-c(slides,
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.ol==0 & in.ul==0 & in.wl==0) {
			slides<-c(slides,paste("<ul>",
							"<ul>",
							"<li>",
							substring(txt[i],3),
							"</li>",sep=""))
		}
		in.wl<-in.slide<-1
		in.ul<-0
		in.ol<-0
	}

	if(ch=="o") {
		if((in.ol==0) & (in.ul==0)) {
			slides<-c(slides,"<ol>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if((in.ol==0) & (in.ul==1)) {
			slides<-c(slides,"</ul>","<ol>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.ol==1) {
			slides<-c(slides,
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		if(in.wl==1) {
			slides<-c(slides,"</ul>","</li>","</ul>","<ol>",
				paste("<li>",substring(txt[i],3),"</li>",sep=""))
		}
		in.ol<-in.slide<-1
		in.ul<-in.wl<-0
	}

	if(ch=="p") {
		if(in.ul==1) { # as above, in.ul and in.ol can't both be 1
			slides<-c(slides,"</ul>",
				paste("<p>",substring(txt[i],3),"</p>",sep=""))
		}
		if(in.ol==1) {
			slides<-c(slides,"</ol>",
				paste("<p>",substring(txt[i],3),"</p>",sep=""))
		}
		if((in.ol==0) & (in.ul==0)) {
			slides<-c(slides,
				paste("<p>",substring(txt[i],3),"</p>",sep=""))
		}
		if(in.wl==1) {
			slides<-c(slides,"</ul>","</li>","</ul>",
				paste("<p>",substring(txt[i],3),"</p>",sep=""))
		}
		in.slide<-1
		in.ol<-in.ul<-0
	}

# i tag should be followed by at least three 'words' separated by spaces
# first, the height in pixels, or h
# second, the width in pixels, or w
# third (and to the end), the image URL, which can contain spaces and should not have quotes
	if(ch=="i") { # image
		iparse<-strsplit(substring(txt[i],3)," ",fixed=TRUE)[[1]]
		ifile<-paste(iparse[3:length(iparse)],collapse=" ")
		check1<-suppressWarnings(is.numeric(as.numeric(iparse[1])) & !(is.na(as.numeric(iparse[1]))))
		check2<-suppressWarnings(is.numeric(as.numeric(iparse[2])) & !(is.na(as.numeric(iparse[2]))))
		if((iparse[1]=="h") & (check2)) {
			slides<-c(slides,
				paste("<image src=\"",ifile,"\" width=",as.numeric(iparse[2]),">",sep=""))
			in.slide<-1
			in.ol<-in.ul<-0
		}
		if((iparse[2]=="w") & (check1)) {
			slides<-c(slides,
				paste("<image src=\"",ifile,"\" height=",as.numeric(iparse[1]),">",sep=""))
			in.slide<-1
			in.ol<-in.ul<-0
		}
		if(check1 & check2) {
			slides<-c(slides,
				paste("<image src=\"",ifile,
					"\" height=",as.numeric(iparse[1]),
					" width=",as.numeric(iparse[2]),">",sep=""))
			in.slide<-1
			in.ol<-in.ul<-0
		}
	}
}

if(is.na(title)) {
	title<-mytitle
}
if(is.na(author)) {
	author<-myauthor
}
prelims<-c("<!DOCTYPE html>",
		"<html>",
		"<head>",
		"<meta charset=\"utf-8\">",
		paste("<title>",title,"</title>",sep=""),
		paste("<meta name=\"description\" content=\"",title,"\">",sep=""),
		paste("<meta name=\"author\" content=\"",author,"\">",sep=""),
		"<link rel=\"stylesheet\" href=\"core/deck.core.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/goto/deck.goto.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/menu/deck.menu.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/navigation/deck.navigation.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/status/deck.status.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/hash/deck.hash.css\">",
		"<link rel=\"stylesheet\" href=\"extensions/scale/deck.scale.css\">",
		paste("<link rel=\"stylesheet\" href=\"themes/style/",style,".css\">",sep=""),
		"<link rel=\"stylesheet\" href=\"themes/transition/fade.css\">",
		"<script src=\"modernizr.custom.js\"></script>",
		"</head>",
		" ",
		"<body class=\"deck-container\">")

endpiece<-c("<!-- End slides. -->",
		"<!-- deck.navigation snippet -->",
		"<a href=\"#\" class=\"deck-prev-link\" title=\"Previous\">&#8592;</a>",
		"<a href=\"#\" class=\"deck-next-link\" title=\"Next\">&#8594;</a>",
		"",
		"<!-- deck.status snippet -->",
		"<p class=\"deck-status\">",
		"<span class=\"deck-status-current\"></span>",
		"/",
		"<span class=\"deck-status-total\"></span>",
		"</p>",
		"",
		"<!-- deck.goto snippet -->",
		"<form action=\".\" method=\"get\" class=\"goto-form\">",
		"<label for=\"goto-slide\">Go to slide:</label>",
		"<input type=\"text\" name=\"slidenum\" id=\"goto-slide\" list=\"goto-datalist\">",
		"<datalist id=\"goto-datalist\"></datalist>",
		"<input type=\"submit\" value=\"Go\">",
		"</form>",
		"",
		"<!-- Required JS files. -->",
		"",
		"<script src=\"jquery-1.7.2.min.js\"></script>",
		"<script src=\"core/deck.core.js\"></script>",
		"",
		"<!-- Extension JS files. Add or remove as needed. -->",
		"<script src=\"core/deck.core.js\"></script>",
		"<script src=\"extensions/hash/deck.hash.js\"></script>",
		"<script src=\"extensions/menu/deck.menu.js\"></script>",
		"<script src=\"extensions/goto/deck.goto.js\"></script>",
		"<script src=\"extensions/status/deck.status.js\"></script>",
		"<script src=\"extensions/navigation/deck.navigation.js\"></script>",
		"<script src=\"extensions/scale/deck.scale.js\"></script>",
		"",
		"<!-- Initialize the deck. You can put this in an external file if desired. -->",
		"<script>",
		"$(function() {",
		"$.deck('.slide');",
		"});",
		"</script>",
		"</body>",
		"</html>")

allhtml<-c(prelims,slides,endpiece)
fout<-file(htmlfile)
writeLines(allhtml,fout)
close(fout)
}


makedeck(htmlfile="ccu2015slides.html",txtfile="ccu2015markdown.txt",style="rlg")