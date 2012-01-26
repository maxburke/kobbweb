var NULL_ID = "00000000000000000000000000000000";

var kw = {
    Models : { },
    App : { },
    Detail : { },
    Views : { },
    Collections : { },
    Data : { },
    ItemCache : { },
    ViewStack : null,
    init : function() { return null; }
};

var ItemCache = {};
var DataCache = {};

memoizeData = function(ref, data) {
    DataCache[ref] = data;
}

post = function(url, returnType, data, onSuccess, onError) {
    var postObject = {
        url : url,
        type : 'POST',
        data : data,
        dataType : returnType, 
        processData : false,
        success : onSuccess,
        error : onError
    };
    $.ajax(postObject);
}

getJson = function(url, onSuccess, onError) {
    var get = { 
        url : url,
        type : 'GET',
        dataType : 'json',
        processData : false,
        success : onSuccess,
        error : onError
    };
    $.ajax(get);
}

getData = function(ref, callback) {
    if (typeof DataCache[ref] === 'undefined') {
        var dataRequest = {
            url : '/data/' + ref,
            type : 'GET',
            dataType : 'text',
            processData : false,
            success : function(dataResult, textStatus, jqXHR) {
                DataCache[ref] = dataResult;
                callback(dataResult);
            }
        };
        $.ajax(dataRequest);
    } else {
        callback(DataCache[ref]);
    }
}

memoizeItem = function(item) {
    ItemCache[item.id] = item;
}

getItem = function(uuid, callback) {
    if (typeof ItemCache[uuid] === 'undefined') {
        var ajaxRequest = { 
            url : '/content/' + uuid,
            type : 'GET',
            dataType : 'json',
            processData : false,
            success : function(data, textStatus, jqXHR) {
                getData(data.contentRef, function(dataResult, textStatus, jqXHR) {
                    data.id = uuid;
                    data.content = dataResult;
                    ItemCache[uuid] = data;
                    callback(data);
                });
            }
        };
        $.ajax(ajaxRequest);
    } else {
        callback(ItemCache[uuid]);
    }
}

kw.Models.Item = Backbone.Model.extend({
    clear : function() {
        this.destroy();
        this.view.remove();
        return null;
    }
});

kw.Collections.ItemCollection = Backbone.Collection.extend({
    model : kw.Models.Item,
    comparator : function(item) { 
        return item.get("idx");
    },
    url : '/content'
});

kw.Views.newItemView = Backbone.View.extend({
    className : 'newItem',
    events : {
        'click .submit' : 'submitNewItem'
    },
    initialize : function() {
        _.bindAll(this, 'render', 'submitNewItem', 'submitSuccess', 'submitError');
    },
    setParent : function(parentView, parentUuid) {
        this.parentView = parentView;
        this.parentUuid = parentUuid;
    },
    render : function() {
        $(this.el).append('<form><fieldset>'
            +'<div class="clearfix">'
            +    '<label for="autosize">Post a new item!</label>'
            +     '<div class="input">'
            +         '<textarea class="autosize" cols="80" rows="2"></textarea>'
            +     '</div>'
            + '</div></fieldset></form>');
        $(this.el).append('<div class="actions"><button class="btn primary submit" type="button">Submit</button></div>');
        this.$('.autosize').autoGrow();
        return this;
    },
    submitError : function (obj) {
        this.$('.autosize').removeAttr('disabled');
        this.$('.submit').removeAttr('disabled');
        alert("error!");
    },
    submitSuccess : function(modelData) {
        this.parentView.addNewChild(modelData);

        this.$('.autosize').removeAttr('disabled');
        this.$('.submit').removeAttr('disabled');
    },
    submitNewItem : function() {
        this.$('.autosize').attr('disabled', true);
        this.$('.submit').attr('disabled', true);

        var obj = this;
        var requestText = this.$('.autosize').val();
        var submitSuccess = this.submitSuccess;
        var dataSubmitData = JSON.stringify({ 
            data : requestText
        });
        var parentUuid = this.parentUuid;
        post('/data', 'text', dataSubmitData, function(data, textStatus, jqXHR) {
                var contentSubmitData = JSON.stringify({ data : data });
                memoizeData(data, requestText);
                post('/content/' + parentUuid, 'json', contentSubmitData, function(returnedData, textStatus, jqXHR) {
                    returnedData.content = requestText;
                    submitSuccess(returnedData);
                    });
                });
    }
});

kw.Views.itemDetailView = Backbone.View.extend({
    el : '#app',
    initialize : function(model) {
        _.bindAll(this, 
            'close', 
            'render',
            'addNewChild',
            'renderChildren',
            'renderParent',
            'renderNav', 
            'aliasSubmit',
            'issueAsyncDataFetches');

        this.model = model;
        this.parentUuid = model.get("parent");

        this.items = new kw.Collections.ItemCollection;
    },
    close : function() {
        $(this.el).empty();
    },
    addNewChild : function(modelData) {
        modelData.idx = this.items.length;
        this.items.add(modelData);
        var parentElement = $('#children');
        var view = new itemSummaryView({ model : this.items.at(modelData.idx) });
        var child = view.render({ deletable : true }).el;
        parentElement.prepend(child);
    },
    renderChildren : function() {
        $(this.el).append('<div class="row">'
                + '<div>'
                +     '<ul id="children"></ul>'
                + '</div></div>');

        var children = this.model.get("children");
        if (children !== null) {
            var childrenElement = $('#children');

            for (var i = 0; i < children.length; ++i) {
                getItem(children[i], function(childModel) {
                    var model = new kw.Models.Item(childModel);
                    var childView = new itemSummaryView({ model : model });
                    childrenElement.append(childView.render({ deletable : true }).el);
                });
            }
        }
    },
    renderParent : function() {
        var element = this.el;
        $(element).append('<div id="parent"><hr/><ul></ul></div>');
        getItem(this.model.get("parent"), function(parentModel) {
            if (parentModel.content.length > 0) {
                var parentElement = $('#parent > ul');
                var model = new kw.Models.Item(parentModel);
                var parentView = new itemSummaryView({ model : model });
                parentElement.append(parentView.render().el);
            } else {
                $('#parent').remove();
            }
        });
    },
    renderAclModal : function() {
        var html = '<div id="acl" class="modal hide fade" style="display: none">';
        html += '<div class="modal-header"><a href="#" class="close">&times;</a>Access</div>';
        html += '<div class="modal-body">Placeholder...</div>';
        html += '<div class="modal-footer">Footer placeholder...</div>';
        html += '</div>';
        return html; 
    },
    renderAliasModal : function() {
        var html = '<div id="alias" class="modal hide fade" style="display: none">';
        html += '<div class="modal-header"><a href="#" class="close">&times;</a>Email Alias</div>';
        html += '<div class="modal-body"><input id="alias-input" class="xlarge" placeholder="fetching...""/>@kobbweb.net</div>';
        html += '<div class="modal-footer"><button type="button" class="btn primary" id="alias-submit">Submit</button></div>';
        html += '</div>';
        return html;
    },
    renderNav : function() {
        var html = '<ul>';
        html += this.renderAliasModal();
        html += this.renderAclModal();
        html += '<li class="itemAction"><a data-controls-modal="alias" data-backdrop="true" data-keyboard="true"><img src="/static/icons/at_20x20.png"></a></li>';
        html += '<li class="itemAction"><a data-controls-modal="links" data-backdrop="true" data-keyboard="true"><img src="/static/icons/links_20x20.png"></a></li>';
        html += '<li class="itemAction"><a data-controls-modal="acl" data-backdrop="true" data-keyboard="true"><img src="/static/icons/acl_20x20.png"></a></li>';
        html += '</ul>';

        return html;
    },
    aliasSubmit : function() {
        var data = JSON.stringify({ alias : $('#alias-input').val()});
        var modalInput = this.$('#alias-submit');
        modalInput.attr('disabled', true);
        var modalElement = $('#alias');
        post('/alias/' + this.model.get("id"),
            'json',
            data,
            function(data, textStatus, jqXHR) {
                modalInput.removeAttr('disabled');
                modalElement.modal('hide');
            },
            function(jqXHR, textStatus, errorThrown) {
                modalInput.removeAttr('disabled');
            });
    },
    issueAsyncDataFetches : function() {
        var aliasElement = this.$('#alias-input');
        this.$('#alias-submit').click(this.aliasSubmit);
        getJson('/alias/' + this.model.get("id"), function(data, textStatus, jqXHR) {
            aliasElement.val(data.alias);
            });
    },
    render : function() {
        if (this.model.get("parent") !== NULL_ID) {
            this.renderParent();
        }

        var html = '<hr/>';
        html += this.renderNav();
        html += '<ul><li><pre class="detail">';
        html += this.model.get("content");
        html += '</pre></li></ul>';
        $(this.el).append(html);

        var newItemView = new kw.Views.newItemView;
        newItemView.setParent(this, this.model.get("id"));
        var html = newItemView.render().el;
        $(this.el).append(html);

        // Issue fetches for data after our code has been inserted into the DOM.
        this.issueAsyncDataFetches();

        this.renderChildren();

        return this;
    }
});

kw.init = function() {
    var uriPath = window.location.pathname;
    var locationLength = uriPath.length;
    var i = locationLength - 1;
    var itemUuid = NULL_ID;
    for (; i !== 0; --i) {
        if (uriPath[i] == '/') {
            itemUuid = uriPath.substr(i + 1);
            break;
        }
    }
    if (itemUuid == NULL_ID)
        alert("Something went wrong!");

    getItem(itemUuid, function(data) {
        var model = new kw.Models.Item(data);
        kw.App = new kw.Views.itemDetailView(model);
        kw.App.render();
    });
}

