const Sortable = require('sortablejs')

console.log(Sortable.create);

window.sortable = Sortable

module.exports.Data = {
    init: function() {
        this.data = this.opts.data
    },
    renew: function(data) {
        (this.opts.renew ||
         this.parent && this.parent.onrenew ||
         this.parent.parent && this.parent.parent.onrenew)(data)
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
module.exports.Sortable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.list, {
                group: this.name,
                sort: true,
                animation: 100,
                dragClass: "drag",
                onStart: e => {
                    e.item.drag_data = this.data[e.oldIndex]
                },
                onSort: e => {
                    const newData = this.data.slice()
                    if (e.from == this.refs.list) newData.splice(e.oldIndex, 1)
                    if (e.to   == this.refs.list) newData.splice(e.newIndex, 0, e.item.drag_data)
                    this.onsort(newData)
                },
            })
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
                    put:  [this.name, 'trash']
                },
                sort: false,
                dragClass: "drag",
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
            this.refs.slot.ondragover = e => {
                if (!this.hover) {
                    this.hover = true
                    this.update()
                }
            }
            this.refs.slot.ondragleave = e => {
                this.hover = false
                this.update()
            }
        })
    },
    onremove: function() {},
    ondrop: function(newData) {}
}
module.exports.Clonable = {
    init: function() {
        this.on('mount', () => {
            this.sortable = Sortable.create(this.refs.clone, {
                group: {
                    name: this.name,
                    pull: 'clone',
                    put: [this.name, 'trash']
                },
                sort: false,
                dragClass: "drag",
                onStart: e => {
                    e.item.drag_data = this.data
                },
            })
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
                    console.log('trash', e);
                },
            })
        })
    },
}
