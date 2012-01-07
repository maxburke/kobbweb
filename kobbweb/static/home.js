
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
    init : function() { return null; }
};

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

kw.Views.itemDetailView = Backbone.View.extend({
    el : '#app',
    initialize : function(model) {
        _.bindAll(this, 'close', 'render');
        this.model = model;
        this.items = new kw.Collections.ItemCollection;
        this.items.bind('add', this.addEntry, this);
        this.items.bind('reset', this.addAllEntries, this);
        this.items.bind('all', this.render, this);

        // Create a 'stack' of views from which we can create breadcrumbs later.
        this.previousView = kw.App;
        kw.App = this;

        this.render();
    },
    close : function() {
        $(this.el).empty();
        kw.App = this.previousView;
        kw.App.render();
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
        $(this.el).append('<button type="button" id="close" class="btn primary">Close</button>');
        $('#close').click(this.close);
        if (this.model.get("parent") !== NULL_ID) {
            this.renderParent();
        }
        $(this.el).append('<div class="row"><div class="span1"></div><div id="detail" class="span15">detail goes here</div></div>');
        this.renderChildren();
    }
});

kw.Views.itemSummaryView = Backbone.View.extend({
    tagName : 'li',
    template : _.template(kw.Templates.itemSummaryViewTemplate),
    events : {
        'dblclick .item-summary' : 'explodeItem'
    },
    explodeItem : function() {
        kw.App.close();
        kw.Detail = new kw.Views.itemDetailView(this.model);
    },
    render : function () {
        var jsonModel = this.model.toJSON();
        var template = this.template(jsonModel);
        $(this.el).html(template);
        return this;
    }
});

kw.Views.AppView = Backbone.View.extend({
    el : '#app',
    initialize : function() {
        _.bindAll(this, 'addEntries', 'close', 'render');
        this.items = new kw.Collections.ItemCollection;

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
            this.render();
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
    addEntries : function(itemList) {
        this.expectedNumModels = itemList.length;
        this.numModelsLoaded = 0;
        for (var i = 0; i < itemList.length; ++i) {
            this.fetchEntry(itemList, i);
        }
    }
});

kw.init = function() {
    kw.App = new kw.Views.AppView();
}

