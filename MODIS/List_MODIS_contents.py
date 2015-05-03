import ftplib,urllib,os,time,sys

def List_MODIS_products(site="http://e4ftl01.cr.usgs.gov/",satellite="MOTA"):

    """
    Lists MODIS products at [http://e4ftl01.cr.usgs.gov] or anywhere else
    
    Can supply satellite as arguement
    MOTA = Terra + Aqua
    MOLA = Aqua only
    MOLT = Terra only
    
    """
    
    website = urllib.urlopen(site+satellite)
    string=website.readlines()
    
    files=[]
    for line in string:
        try:files.append(line.replace('/','').split('"')[5])
        except: pass
    return files

# So what do we have on offer?
print( List_MODIS_products() )