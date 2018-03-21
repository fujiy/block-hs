// Expr

import I from 'Block.Bridge'

<bind>
  <bind-left data={opts.data.value0}/>
  <span class='token'>=</span>
  <expr data={opts.data.value1} outer={true}/>

  console.log(this.opts.data);
</bind>

<bind-left>
  <div class='term var'>
    <span class='token'>{var.value0.value0}</span>
  </div>
  <script>
    this.args = I.appToArray(this.opts.data)
    this.var = this.args.shift()
  </script>
</bind-left>

<expr-slot>
  <var-expr if={cons == 'var'} data={expr}/>
  <!-- <app-expr if={cons == 'app'} data={expr}/> -->
  <lit-expr if={cons == 'lit'} data={expr}/>
  <script>
    this.expr = opts.data.value0
    this.cons = I.econs(this.expr)
  </script>
</expr-slot>

<expr class={left: opts.left, right: opts.right, outer: outer}>
  <var-expr if={cons == 'var'} data={expr}/>
  <app-expr if={cons == 'app'} data={expr} spine={spine} outer={outer}/>
  <num-expr if={cons == 'num'} data={expr}/>

  <div class='type-info' hidden={!hover}><scheme data={scheme}/></div>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.expr   = opts.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = opts.data.value1
    this.spine  = opts.spine || ""
    this.outer  = opts.outer || (this.cons == 'app' && !opts.spine)
    this.hover  = false

    if (this.cons == 'app' && !this.spine) {
        let es = I.appToArray(opts.data)
        this.spine = I.econs(es[0].value0)
    }

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
  </script>
</expr>

<highlight class={outer: opts.outer, hover: opts.hover}>

</highlight>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
</var-expr>

<app-expr class='{opts.spine} {opts.outer?"outer":""}'>
  <expr data={opts.data.value0} spine={opts.spine} left={true}/>
  <expr data={opts.data.value1} right={opts.outer}/>
</app-expr>

<num-expr class='term num'>
  <span class='token'>{opts.data.value0}</span>
</num-expr>

<stud-left class='{cons}'>
  <script>
    this.cons = I.econs(this.opts.data)
  </script>
</stud-left>

<stud-right class='{cons}'>
<script>
  this.cons = I.econs(this.opts.data)
</script>
</stud-right>
