const Sortable = require('sortablejs')

console.log(Sortable.create);

window.sortable = Sortable

module.exports.Data = {
    init: function() {
        this.data = this.opts.data
    },
    renew: function(data) {
        // (this.opts.renew ||
        //  this.parent && this.parent.onrenew ||
        //  this.parent.parent && this.parent.parent.onrenew)(data)
        (this.opts.renew ||
         this.parent && this.parent.onrenew)(data)
    },
    onrenew: function(data){ this.renew(data) } // override this
};
module.exports.Selectable = {
    init: function() {
        this.hover = false
        this.root.onmouseover = e => {
            this.hover = true
            this.update()
            e.stopPropagation()
        }
        this.root.onmouseout = e => {
            this.hover = false
            this.update()
            e.stopPropagation()
        }
    }
}
function setSelectEvent(self, target) {
    target.ondragover = e => {
        if (!self.hover) {
            self.hover = true
            self.update()
        }
    }
    target.ondragleave = e => {
        self.hover = false
        self.update()
  }
}

module.exports.Sortable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.list, {
                group: this.name,
                sort: true,
                animation: 100,
                dragClass: "drag",
                ghostClass: "ghost",
                onStart: e => {
                    e.item.drag_data = this.listData[e.oldIndex]
                },
                onSort: e => {
                    const newData = this.listData.slice()
                    if (e.from == this.refs.list) newData.splice(e.oldIndex, 1)
                    if (e.to   == this.refs.list) newData.splice(e.newIndex, 0, e.item.drag_data)
                    this.onsort(newData)
                },
            })
            setSelectEvent(this, this.refs.list)
        })
    },
    onsort: function(newData){}
}
module.exports.Draggable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.slot, {
                group: {
                    name: this.name,
                    put:  [this.name]
                },
                sort: false,
                dragClass: "drag",
                ghostClass: "ghost",
                onStart: e => {
                    e.item.drag_data = this.data
                    this.onremove()
                },
                onSort: e => {
                    const newData = e.item.drag_data
                    // console.log(e, e.to == this.refs.slot);
                    if (e.to == this.refs.slot) this.ondrop(newData)
                },
            })
            setSelectEvent(this, this.refs.slot)
        })
    },
    onremove: function() {},
    ondrop: function(newData) {}
}
module.exports.Droppable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.slot, {
                group: {
                    name: this.name,
                    pull: false,
                    put:  [this.name]
                },
                sort: false,
                ghostClass: "ghost",
                onSort: e => {
                    const newData = e.item.drag_data
                    if (e.to == this.refs.slot) this.ondrop(newData)
                },
            })
            setSelectEvent(this, this.refs.slot)
        })
    },
    ondrop: function(data) {}
}
module.exports.Clonable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.slot, {
                group: {
                    name: this.name,
                    pull: 'clone',
                    put: false
                },
                sort: false,
                dragClass: "drag",
                ghostClass: "ghost",
                filter: 'input',
                preventOnFilter: false,
                onStart: e => {
                    e.item.drag_data = this.data
                },
            })
            setSelectEvent(this, this.refs.slot)
        })
    }
}
module.exports.Trash = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.area, {
                group: 'expr',
                sort: false,
                // draggable: '',
                onSort: e => {
                    e.item.remove()
                    // console.log('trash', e.item, e.clone);
                },
            })
        })
    },
}
