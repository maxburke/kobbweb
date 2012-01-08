
var ITEM_SUMMARY_VIEW_TEMPLATE = '<div class="item-summary">'
+ '<div><%= content %></div>'
+ '<div><%= (children !== null ? children.length : 0) %></div>'
+ '</div>';

var NULL_ID = "00000000000000000000000000000000";

var kw = {
    Models : { },
    App : { },
    Detail : { },
    Views : { },
    Collections : { },
    Templates : { itemSummaryViewTemplate : ITEM_SUMMARY_VIEW_TEMPLATE },
    Data : { },
    ItemCache : { },
    ViewStack : null,
    init : function() { return null; }
};

viewStackPush = function(view) {
    if (kw.ViewStack !== null) {
        kw.ViewStack.close();
        $('#back').removeAttr('disabled');
    }

    view.nextView = kw.ViewStack;
    kw.ViewStack = view;
    kw.ViewStack.render();
}

viewStackPop = function() {
    var topElement = kw.ViewStack;
    kw.ViewStack.close();
    kw.ViewStack = kw.ViewStack.nextView;
    kw.ViewStack.render();

    if (kw.ViewStack.nextView === null) {
        $('#back').attr('disabled', true);
    }

    return topElement;
}

viewStackPeek = function () {
    return kw.ViewStack;
}

fetchData = function(contentRef, callback) {
    if (typeof kw.Data[contentRef] === 'undefined') {
        var dataRequest = {
            url : '/data/' + contentRef,
            type : 'GET',
            dataType : 'text',
            processData : false,
            success : function(dataResult, textStatus, jqXHR) {
                kw.Data[contentRef] = dataResult;
                callback(dataResult, textStatus, jqXHR);
            }
        };
        $.ajax(dataRequest);
    } else {
        callback(kw.Data[contentRef], null, null);
    }
}

addToDataCache = function(contentRef, data) {
    kw.Data[contentRef] = data;
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

kw.ItemCache = new kw.Collections.ItemCollection;

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
            +         '<textarea class="autosize" cols="80" rows="5"></textarea>'
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
        kw.ItemCache.add(modelData);
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
        var dataPost = {
            url : '/data',
            type : 'POST',
            text : 'json',
            data : dataSubmitData,
            processData : false,
            success : function (data, textStatus, jqXHR) {
                var contentSubmitData = JSON.stringify({
                    data : data,
                });
                addToDataCache(data, requestText);
                var contentPost = {
                    url : '/content/' + parentUuid,
                    type : 'POST',
                    dataType : 'json',
                    data : contentSubmitData,
                    processData : false,
                    success : function(returnedData, textStatus, jqXHR) {
                        returnedData.content = requestText;
                        submitSuccess(returnedData);
                    }
                };
                $.ajax(contentPost);
            }
        };
        $.ajax(dataPost);
        // submit item
        // send message to parentView to add new child to its collection.
        // I need to better think of the data + data flows here methinks.???
    }
});

kw.Views.itemDetailView = Backbone.View.extend({
    el : '#app',
    initialize : function(model) {
        _.bindAll(this, 'close', 'render', 'addNewChild');

        this.model = model;
        this.parentUuid = model.get("parent");

        this.items = new kw.Collections.ItemCollection;
        this.items.bind('add', this.addEntry, this);
        this.items.bind('reset', this.addAllEntries, this);
        this.items.bind('all', this.render, this);

        viewStackPush(this);
    },
    close : function() {
        $(this.el).empty();
    },
    addNewChild : function(modelData) {
        modelData.idx = this.items.length;
        this.items.add(modelData);
        var parentElement = $('#children');
        var view = new kw.Views.itemSummaryView({ model : this.items.at(modelData.idx) });
        var child = view.render().el;
        parentElement.prepend(child);
    },
    renderChildren : function() {
        var children = this.model.get("children");
        if (children !== null) {
            $(this.el).append('<div class="row"><div class="span2"></div><div class="span14">children go here<ul id="children"></ul></div></div>');
            var childrenElement = $('#children');
            for (var i = 0; i < children.length; ++i) {
                var childModel = kw.ItemCache.get(children[i]);
                var childView = new kw.Views.itemSummaryView({ model : childModel });
                childrenElement.append(childView.render().el);
            }
        }
    },
    renderParent : function() {
        $(this.el).append('<div class="row"><div class="span16">parent goes here<ul id="parent"</ul></div></div>');
        var parentElement = $('#parent');
        var parentModel = kw.ItemCache.get(this.model.get("parent"));
        var parentView = new kw.Views.itemSummaryView({ model : parentModel});
        parentElement.append(parentView.render().el);
    },
    render : function() {
        if (this.model.get("parent") !== NULL_ID) {
            this.renderParent();
        }

        $(this.el).append('<div class="row"><div class="span1"></div><div id="detail" class="span15">detail goes here</div></div>');

        var newItemView = new kw.Views.newItemView;
        newItemView.setParent(this, this.model.get("id"));
        var html = newItemView.render().el;
        $(this.el).append(html);

        this.renderChildren();

        return this;
    }
});

kw.Views.itemSummaryView = Backbone.View.extend({
    tagName : 'li',
    template : _.template(kw.Templates.itemSummaryViewTemplate),
    events : {
        'dblclick .item-summary' : 'explodeItem'
    },
    explodeItem : function() {
        kw.Detail = new kw.Views.itemDetailView(this.model);
    },
    render : function () {
        if (typeof this.model === 'undefined') {
            alert("undefined");
        }

        var jsonModel = this.model.toJSON();
        var template = this.template(jsonModel);
        $(this.el).html(template);
        $(this.el).twipsy({ animate: false, delayIn : 500, fallback : 'Double-click for detail view', placement : 'below', title : function() { return 'Double-click for detail view'; } });
        return this;
    }
});

kw.Views.AppView = Backbone.View.extend({
    el : '#app',
    initialize : function() {
        _.bindAll(this, 'addEntries', 'close', 'render', 'addNewChild');
        this.items = new kw.Collections.ItemCollection;
        this.parentUuid = NULL_ID;

        var ajaxRequest = {
            url : '/posts',
            type : 'GET',
            dataType : 'json',
            processData : false,
            success : function(data, textStatus, jqXHR) { kw.App.addEntries(data); }
        };
        $.ajax(ajaxRequest);
    },
    close : function() {
        $(this.el).empty();
    },
    render : function() {
        $(this.el).append('<h3>Post new item!</h3>');
        var newItemView = new kw.Views.newItemView;
        newItemView.setParent(this, NULL_ID);
        var html = newItemView.render().el;
        $(this.el).append(html);

        $(this.el).append('<h3>Most recent items</h3>');
        $(this.el).append('<ul id="post-list"></ul>');
        var parentElement = $('#post-list');

        for (var i = 0; i < this.items.length; ++i) {
            var view = new kw.Views.itemSummaryView({ model : this.items.at(i) });
            var child = view.render().el;
            parentElement.append(child);
        }
    },
    modelLoadedCallback : function() {
        if (++this.numModelsLoaded === this.expectedNumModels) {
            viewStackPush(this);
        }
    },
    fetchEntry : function(itemList, i) {
        var collection = this.items;
        var view = this;
        var ajaxRequest = { 
            url : '/content/' + itemList[i],
            type : 'GET',
            dataType : 'json',
            processData : 'false',
            success : function(data, textStatus, jqXHR) {
                fetchData(data.contentRef, function(dataResult, textStatus, jqXHR) {
                    data.idx = i;
                    data.id = itemList[i];
                    data.content = dataResult;
                    collection.add(data);
                    kw.ItemCache.add(data);
                    view.modelLoadedCallback();
                });
            }
        };
        $.ajax(ajaxRequest);
    },
    addEntry : function(model) {
        alert("add an entry here. you might need to change the collection comparator here to sort in reverse order. "
            + "THEN change the idx's assigned to the entries to be (numItems - i) instead of i so that new entries "
            + "are added to the right part of the array and, when sorted end up appearing at the top of the list.");
    },
    addEntries : function(itemList) {
        this.expectedNumModels = itemList.length;
        this.numModelsLoaded = 0;
        for (var i = 0; i < itemList.length; ++i) {
            this.fetchEntry(itemList, i);
        }
    },
    addNewChild : function(modelData) {
        modelData.idx = this.items.length;
        this.items.add(modelData);
        var parentElement = $('#post-list');
        var view = new kw.Views.itemSummaryView({ model : this.items.at(modelData.idx) });
        var child = view.render().el;
        parentElement.prepend(child);
    }
});

kw.init = function() {
    kw.App = new kw.Views.AppView();
    $('#back').click(viewStackPop);
}

