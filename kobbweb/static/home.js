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
                memoizeData(data, requestText);
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
            var view = new itemSummaryView({ model : this.items.at(i) });
            var child = view.render().el;
            parentElement.append(child);
        }
    },
    addEntry : function(model) {
        alert("add an entry here. you might need to change the collection comparator here to sort in reverse order. "
            + "THEN change the idx's assigned to the entries to be (numItems - i) instead of i so that new entries "
            + "are added to the right part of the array and, when sorted end up appearing at the top of the list.");
    },
    addEntries : function(itemList) {
        if (itemList === null || itemList.length === 0) {
            this.render();
            return;
        }

        var itemsLength = itemList.length;
        var dataRefsFetched = 0;
        var collection = this.items;
        var renderFn = this.render;

        for (var i = 0; i < itemsLength; ++i) {
            var callback = (function(index) { return function(dataResult) {
                ++dataRefsFetched;
                var item = itemList[index];
                item.content = dataResult;
                item.idx = index;
                ItemCache[item.id] = item;
                collection.add(item);

                if (dataRefsFetched === itemsLength) {
                    renderFn();
                }
            }})(i);
            getData(itemList[i].contentRef, callback);
        }
    },
    addNewChild : function(modelData) {
        modelData.idx = this.items.length;
        this.items.add(modelData);
        var parentElement = $('#post-list');
        var view = new itemSummaryView({ model : this.items.at(modelData.idx) });
        var child = view.render().el;
        parentElement.prepend(child);
    }
});

kw.init = function() {
    kw.App = new kw.Views.AppView();
}
