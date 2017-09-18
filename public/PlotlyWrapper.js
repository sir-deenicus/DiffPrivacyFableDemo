function col_plot (title, xs,ys, divId) {
    var data = [{"type":"bar","x":xs,"y":ys,"orientation":"h"}];
    var layout = {"title":title};
    Plotly.newPlot(divId, data, layout);
}

function bar_plot (title, xs,ys, divId) {
    var data = [{"type":"bar","x":xs,"y":ys}];
    var layout = {"title":title,height: 400,width:650};
    Plotly.newPlot(divId, data, layout);
}

function line_plot(title, xs,ys, divId){
    var data = [{"type":"scatter","x":xs,"y":ys}];
    var layout = {"title":title};
    Plotly.newPlot(divId, data, layout);
}