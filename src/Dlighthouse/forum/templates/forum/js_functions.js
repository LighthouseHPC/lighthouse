dojo.require("dijit.Dialog");
dojo.require("dijit.Editor");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.DateTextBox");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.TextBox");
dojo.require("dijit.form.Textarea");
dojo.require("dijit.form.TimeTextBox");


dojo.addOnLoad(function() {
    var formDlg = dijit.byId("formDialog");
    dojo.connect(dijit.byId("buttonNewTopic"), "onClick", formDlg, "show");
});


function submitThread(title, body){
    console.log(title);
    console.log(body);
}