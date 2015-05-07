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
        print "--------------------"
        print "Total size : "
        print str( round( sum(size),2) ) + " " + "MBs"
        print "Average size :"
        print str( round( sum(size)/len(size),2) ) + " " + "MBs"
    else:
        print "--------------------"
        print "Total size : "
        print str( round( sum(size),2) ) + " " + "GBs"
        print "Average size :"
        print str( round( sum(size)/len(size),2) ) + " " + "GBs"

SizeOnServer(["http://stackoverflow.com/questions/22126078/what-is-the-difference-between-sublime-text-and-githubs-atom"])
