define(['pipAPI', 'https://cdn.jsdelivr.net/gh/baranan/minno-tasks@0.*/IAT/qualtrics/quiat9.js'], function(APIConstructor, iatExtension){
    var API = new APIConstructor();

    var catColor = '#FFFF00';
    var attColor = '#00FFFF';
    var bkgColor = '#3d3e3f';

    return iatExtension({
        canvas : {
            proportions: 0.7 ,
            width:750,
            borderWidth: 0,
            background:bkgColor,
            canvasBackground:bkgColor,
            css: { color: 'white', lineHeight:1.2, fontWeight:400 }
        },
        fontColor:'white',
        keysCss : {'font-size':'0.8em', 'font-family':'courier', color:'white'},
        orCss : {'font-size':'1.8em', color:'white'},
        category1 : {
            name : 'Self', //Will appear in the data.
            title : {
                media : {word : 'Self'}, //Name of the category presented in the task.
                css : {color:catColor,'font-size':'2em'}, //Style of the category title.
                height : 4 //Used to position the "Or" in the combined block.
            }, 
            stimulusMedia : [ //Stimuli content as PIP's media objects
                {word : 'I'}, 
                {word : 'me'}, 
                {word : 'mine'}, 
                {word : 'self'}, 
                {word : 'myself'}
            ], 
            //Stimulus css (style)
            stimulusCss : {color:catColor,'font-size':'1.4em'}
        },	
        category2 :	{
            name : 'Others', //Will appear in the data.
            title : {
                media : {word : 'Others'}, //Name of the category presented in the task.
                css : {color:catColor,'font-size':'2em'}, //Style of the category title.
                height : 4 //Used to position the "Or" in the combined block.
            }, 
            stimulusMedia : [ //Stimuli content as PIP's media objects
                {word : 'others'}, 
                {word : 'they'}, 
                {word : 'them'}, 
                {word : 'their'}, 
                {word : 'theirs'}			], 
            //Stimulus css
            stimulusCss : {color:catColor,'font-size':'1.4em'}
        },
        attribute1 : {
            name : 'extraversion',
            title : {
                media : {word : 'extraversion'},
                css : {color:attColor,'font-size':'1.8em'},
                height : 4 //Used to position the "Or" in the combined block.
            },
            stimulusMedia : [ //Stimuli content as PIP's media objects
                {word: 'sociable'},
                {word: 'talkative'},
                {word: 'active'},
                {word: 'impulsive'},
                {word: 'outgoing'}
            ],
            //Stimulus css
            stimulusCss : {color:attColor,'font-size':'1.4em'}
        },
        attribute2: {
            name : 'introversion',
            title : {
                media : {word : 'introversion'},
                css : {color:attColor,'font-size':'2em'},
                height : 4 //Used to position the "Or" in the combined block.
            },
            stimulusMedia : [ //Stimuli content as PIP's media objects
                {word: 'shy'},
                {word: 'reticent'},
                {word: 'passive'},
                {word: 'deliberate'},
                {word: 'reserved'}
            ],
            //Stimulus css
            stimulusCss : {color:attColor,'font-size':'1.4em'}
        },

        finalText : 'Press space to continue to the next task', 

        instAttributePractice: '<div><p align="center" style="font-size:20px; font-family:arial; color:white;">' +
        '<font color="white"><u>Part blockNum of nBlocks </u><br/><br/></p>' +
        '<p style="font-size:20px; text-align:left; vertical-align:bottom; margin-left:10px; font-family:arial">' +
        'Put a left finger on the <b>E</b> key for items that belong to the category <font color="#0000ff">leftAttribute.</font>' +
        '<br/>Put a right finger on the <b>I</b> key for items that belong to the category <font color="#0000ff">rightAttribute</font>.<br/><br/>' +
        'If you make a mistake, a red <font color="#ff0000"><b>X</b></font> will appear. ' +
        'Press the other key to continue.<br/>' +
        '<u>Go as fast as you can</u> while being accurate.<br/><br/></p>'+
        '<p align="center">Press the <b>space bar</b> when you are ready to start.</font></p></div>',

        instCategoriesPractice: '<div><p align="center" style="font-size:20px; font-family:arial; color:white;">' +
        '<font color="white"><u>Part blockNum of nBlocks </u><br/><br/></p>' +
        '<p style="font-size:20px; text-align:left; vertical-align:bottom; margin-left:10px; font-family:arial">' +
        'Put a left finger on the <b>E</b> key for items that belong to the category <font color="#336600">leftCategory</font>. ' +
        '<br/>Put a right finger on the <b>I</b> key for items that belong to the category <font color="#336600">rightCategory</font>.<br/>' +
        'Items will appear one at a time.<br/><br/>' +
        'If you make a mistake, a red <font color="#ff0000"><b>X</b></font> will appear. ' +
        'Press the other key to continue.<br/>' +
        '<u>Go as fast as you can</u> while being accurate.<br/><br/></p>'+
        '<p align="center">Press the <b>space bar</b> when you are ready to start.</font></p></div>',

        instFirstCombined : '<div><p align="center" style="font-size:20px; font-family:arial; color:white;">' +
        '<font color="white"><u>Part blockNum of nBlocks </u><br/><br/></p>' +
        '<p style="font-size:20px; text-align:left; vertical-align:bottom; margin-left:10px; font-family:arial">' +
        'Use the <b>E</b> key for <font color="#336600">leftCategory</font> and for <font color="#0000ff">leftAttribute</font>.<br/>' +
        'Use the <b>I</b> key for <font color="#336600">rightCategory</font> and for  <font color="#0000ff">rightAttribute</font>.<br/>' +
        'Each item belongs to only one category.<br/><br/>' +
        'If you make a mistake, a red <font color="#ff0000"><b>X</b></font> will appear. ' +
        'Press the other key to continue.<br/>' + 
        '<u>Go as fast as you can</u> while being accurate.<br/><br/></p>' +
        '<p align="center">Press the <b>space bar</b> when you are ready to start.</font></p></div>',

        instSecondCombined : '<div><p align="center" style="font-size:20px; font-family:arial">' +
        '<font color="white"><u>Part blockNum of nBlocks </u><br/><br/></p>' +
        '<p style="font-size:20px; text-align:left; vertical-align:bottom; margin-left:10px; font-family:arial">' +
        'This is the same as the previous part.<br/>' +
        'Use the <b>E</b> key for <font color="#336600">leftCategory</font> and for <font color="#0000ff">leftAttribute</font>.<br/>' +
        'Use the <b>I</b> key for <font color="#336600">rightCategory</font> and for  <font color="#0000ff">rightAttribute</font>.<br/>' +
        'Each item belongs to only one category.<br/><br/>' +
        '<u>Go as fast as you can</u> while being accurate.<br/><br/></p>' +
        '<p align="center">Press the <b>space bar</b> when you are ready to start.</font></p></div>',

        instSwitchCategories : '<div><p align="center" style="font-size:20px; font-family:arial">' +
        '<font color="white"><u>Part blockNum of nBlocks </u><br/><br/></p>' +
        '<p style="font-size:20px; text-align:left; vertical-align:bottom; margin-left:10px; font-family:arial">' +
        '<b>Watch out, the labels have changed position!</b><br/>' +
        'Put the left finger on the <b>E</b> key for <font color="#336600">leftCategory</font>.<br/>' +
        'Put the right finger on the <b>I</b> key for <font color="#336600">rightCategory</font>.<br/><br/>' +
        '<u>Go as fast as you can</u> while being accurate.<br/><br/></p>' +
        '<p align="center">Press the <b>space bar</b> when you are ready to start.</font></p></div>'
    });
});