import urllib, os

def SizeOnServer(urls):
    """
    Returns the total size of online files on a server.
    Expects an array with a list of urls to be downloaded. 
    """
    print "Opening urls on server..."
    size = []
    for link in urls:
        site = urllib.urlopen(link)
        meta = site.info()
        size.append( float( meta.getheaders("Content-Length")[0] ) )
        
    # Convert to MBs
    size = [ x / pow(1024,2) for x in size]
    if sum(size) > 750:
        # Convert to GBs
        size = [ x / 1024 for x in size]
        prettyPrint(size,"GBs")
    else:
        prettyPrint(size)
    

def prettyPrint(size,unit="MBs",r=2):
    print "--------------------"
    print "Total size : "
    print str( round( sum(size),r) ) + " " + unit
    print "Average size :"
    print str( round( sum(size)/len(size),r) ) + " " + unit