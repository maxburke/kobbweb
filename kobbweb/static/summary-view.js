var ITEM_SUMMARY_VIEW_TEMPLATE = '<div class="item-summary">'
+ '<div><pre><%= contentSummary %></pre></div>'
+ '</div>';
var ITEM_SUMMARY_VIEW_DELETABLE_TEMPLATE = '<div class="item-summary">'
+ '<div class="row"><div class="span1"><button class="btn danger delete">&times;</button></div>'
+ '<div class="span12"><pre><%= contentSummary %></pre></div>'
+ '</div></div>';

itemSummaryView = Backbone.View.extend({
    tagName : 'li',
    template : _.template(ITEM_SUMMARY_VIEW_TEMPLATE),
    deletableTemplate : _.template(ITEM_SUMMARY_VIEW_DELETABLE_TEMPLATE),
    events : {
        'dblclick .item-summary' : 'explodeItem',
        'click    .delete' : 'deleteItem'
    },
    initialize : function() {
        _.bindAll(this, 'explodeItem', 'deleteItem', 'render');
    },
    explodeItem : function() {
        window.location = "/x/" + this.model.get("id");
    },
    deleteItem : function() {
        var view = this;
        var submitData = JSON.stringify({
            child : this.model.get("id")
        });
        var deleteRequest = {
            url : '/content/' + this.model.get("parent"),
            type : 'DELETE',
            dataType : 'json',
            data : submitData,
            processData : false,
            success : function(data, textStatus, jqXHR) {
                view.remove();
            }
        };
        $.ajax(deleteRequest);
    },
    render : function (options) {
        if (typeof this.model === 'undefined') {
            alert("undefined");
        }

        var jsonModel = this.model.toJSON();

        if (jsonModel.content.length > 32) {
            jsonModel.contentSummary = jsonModel.content.substr(0, 32);
            jsonModel.contentSummary += "...";
        } else {
            jsonModel.contentSummary = jsonModel.content;
        }

        var template = '';
        if (typeof options !== 'undefined' && options.deletable === true) {
            template = this.deletableTemplate(jsonModel);
        } else {
            template = this.template(jsonModel);
        }

        $(this.el).html(template);
        return this;
    }
});


