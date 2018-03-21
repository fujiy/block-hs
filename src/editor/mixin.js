
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
