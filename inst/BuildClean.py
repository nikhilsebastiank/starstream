#!/usr/bin/env python
# coding: utf-8

# In[2]:


# Preamble

# Make sure the packages below are installed.

# Import the music brains package and set the user agent to query data
# Load the libraries
import musicbrainzngs
import pandas as pd
from random import randint
from time import sleep

# Set user-agent
musicbrainzngs.set_useragent(, )

# User-defined Function to create the first dataframe:

def getartist(j):
    artist_id = j
    result = musicbrainzngs.get_artist_by_id(artist_id,
                  includes=["release-groups"], release_type=["album", "ep"])
    if ('id' in result["artist"]) == True:
        artistid = result["artist"]['id']
    else:
        artistid = "NA"

    if ('type' in result["artist"]) == True:
        artisttype = result["artist"]['type']
    else:
        artisttype = "NA"

    if ('gender' in result["artist"]) == True:
        gender = result["artist"]['gender']
    else:
        gender = "NA"

    if ('name' in result["artist"]) == True:
        name = result["artist"]['name']
    else:
        name = "NA"

    if ('sort-name' in result["artist"]) == True:
        sortname = result["artist"]['sort-name']
    else:
        sortname = "NA"

    if ('disambiguation' in result["artist"]) == True:
        disamb = result["artist"]['disambiguation']
    else:
        disamb = "NA"

    if ('country' in result["artist"]) == True:
        country = result["artist"]['country']
    else:
        country = "NA"

    if ('area' in result["artist"]) == True:
        area=result["artist"]['area']['name']
    else:
        area = "NA"

    if ('begin-area' in result["artist"]) == True:
        beginarea=result["artist"]['begin-area']['name']
    else: 
        beginarea = "NA"

    if ('end-area' in result["artist"]) == True:
        endarea = result["artist"]['end-area']['name']
    else: 
        endarea = "NA"

    if ('life-span' in result["artist"]) == True:
        if ('ended' in result["artist"]['life-span']) == True:
            lifespanbegin=result["artist"]['life-span']['begin']
            lifespanend=result["artist"]['life-span']['end']
        else: 
            lifespanbegin=result["artist"]['life-span']['begin']
            lifespanend = "NA"
    else: 
        lifespanbegin= "NA"
        lifespanend = "NA"

    if ('release-group-count' in result["artist"]) == True:
        releasecount = result["artist"]['release-group-count']
    else:
        releasecount = "NA"  
    data = {'id': [artistid],
            'type': [artisttype],
           'name': [name],
            'gender': [gender],
           'sort-name': [sortname], 
           'disambiguation': [disamb], 
           'country': [country], 
           'area': [area],
           'beginarea': [beginarea],
            'endarea': [endarea],
            'lifespanbegin': [lifespanbegin],
            'lifespanend': [lifespanend],
           'releasecount': [releasecount]}
    df = pd.DataFrame(data)
    return(df)


# In[10]:


keysdata = pd.read_csv("data/artistkeysmain.csv")

def build(a):
    firstcolumn = keysdata.iloc[:,0]
    # Adjust the number of rows here:
    global mainData
    mainData = getartist(firstcolumn[a])
    #secondcolumn = firstcolumn[a+1:a+500]
    for i in firstcolumn[a+1:]:
        artist_id = i
        result = musicbrainzngs.get_artist_by_id(artist_id,
                      includes=["release-groups"], release_type=["album", "ep"])
        if ('id' in result["artist"]) == True:
            artistid = result["artist"]['id']
        else:
            artistid = "NA"

        if ('type' in result["artist"]) == True:
            artisttype = result["artist"]['type']
        else:
            artisttype = "NA"

        if ('gender' in result["artist"]) == True:
            gender = result["artist"]['gender']
        else:
            gender = "NA"

        if ('name' in result["artist"]) == True:
            name = result["artist"]['name']
        else:
            name = "NA"

        if ('sort-name' in result["artist"]) == True:
            sortname = result["artist"]['sort-name']
        else:
            sortname = "NA"

        if ('disambiguation' in result["artist"]) == True:
            disamb = result["artist"]['disambiguation']
        else:
            disamb = "NA"

        if ('country' in result["artist"]) == True:
            country = result["artist"]['country']
        else:
            country = "NA"

        if ('area' in result["artist"]) == True:
            area=result["artist"]['area']['name']
        else:
            area = "NA"

        if ('begin-area' in result["artist"]) == True:
            beginarea=result["artist"]['begin-area']['name']
        else: 
            beginarea = "NA"

        if ('end-area' in result["artist"]) == True:
            endarea = result["artist"]['end-area']['name']
        else: 
            endarea = "NA"

        if ('life-span' in result["artist"]) == True:
            if ('begin' in result["artist"]['life-span']) == True:
                lifespanbegin=result["artist"]['life-span']['begin']
            if ('end' in result["artist"]['life-span']) == True:
                lifespanend=result["artist"]['life-span']['end']
            else: 
                lifespanbegin= "NA"
                lifespanend = "NA"
        else: 
            lifespanbegin= "NA"
            lifespanend = "NA"

        if ('release-group-count' in result["artist"]) == True:
            releasecount = result["artist"]['release-group-count']
        else:
            releasecount = "NA"  

        columns = list(mainData)
        data = []
        values = [artistid, artisttype, name, gender, sortname, disamb, country, area, beginarea, endarea, lifespanbegin, lifespanend, releasecount]
        zipped = zip(columns, values)
        a_dictionnary = dict(zipped)
        a_dictionnary
        data.append(a_dictionnary)
        mainData = mainData.append(data,True)
        sleep(randint(2,4))
        mainData.to_csv("/Volumes/GoogleDrive/My Drive/stardom/data/mainData.csv")


# In[ ]:


# The argument here is the starting row
mainData = build(1650)

