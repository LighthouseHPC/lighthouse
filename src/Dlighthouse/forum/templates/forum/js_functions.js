dojo.require("dijit.Dialog");
dojo.require("dijit.Editor");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.DateTextBox");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.TextBox");
dojo.require("dijit.form.Textarea");
dojo.require("dijit.form.TimeTextBox");
dojo.provide("myValidationTextarea");
dojo.require("dijit.form.SimpleTextarea");
dojo.require("dijit.form.ValidationTextBox");

dojo.addOnLoad(function() {
    var formDlg = dijit.byId("formDialog");
    dojo.connect(dijit.byId("buttonNewTopic"), "onClick", formDlg, "show");
});


dojo.declare(
    "myValidationTextarea",
    [dijit.form.ValidationTextBox,dijit.form.SimpleTextarea],
    {
        invalidMessage: "This field is required",

        postCreate: function() {
            this.inherited(arguments);
        },

        validate: function() {
            this.inherited(arguments);
            if (arguments.length==0) this.validate(true);
        },

        onFocus: function() {
            if (!this.isValid()) {
                this.displayMessage(this.getErrorMessage());
            }
        },

        onBlur: function() {
            this.validate(false);
        }
     }
);


function searchbutcheck_onclick(){
    var x=dojo.byId("id_q").value;
    if (x==null || x==""){
        alert("Oops! The search filed is empty.");
        return false;
    }
    else
        return true;
}