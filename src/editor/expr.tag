// Expr

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Bind ------------------------------------------------------------------------

<bind>
  <bind-left data={data.value0}/>
  <span class='token'>=</span>
  <expr data={data.value1} outer={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.onrenew = d => {
        this.renew(I.renewRight(d)(this.data))
    }
  </script>
</bind>

<bind-left class='var'>
  <func-sample data={var}/>
  <!--<div class='term var {args.length == 0 ?"right":""}'>
    <span class='token'>{var.value0.value0}</span>
</div>-->
  <pattern each={d, i in args} data={d} right={i == args.length - 1}/>

  <type-info show={hover} data={var.value1}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.args = I.appToArray(this.opts.data)
    this.var = this.args.shift()
  </script>
</bind-left>

// Expr ------------------------------------------------------------------------

<expr class={left: opts.left, right: opts.right, outer: outer, func: func, bracket: bracket}>
  <div ref='slot' class='slot'>
    <expr-emp if={cons == 'emp'} data={scheme.value1}/>
    <var-expr if={cons == 'var'} data={expr}/>
    <app-expr if={cons == 'app'} data={expr} spine={spine} outer={outer}/>
    <num-expr if={cons == 'num'} data={expr}/>
  </div>
  <hole if={outer && func} spine={spine} each={t, i in holes}
        data={t} right={i == holes.length - 1} renew={apply(i)}/>

  <type-info show={hover} data={scheme}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Draggable)

    this.expr    = this.data.value0
    this.cons    = I.econs(this.expr)
    this.scheme  = this.data.value1
    this.holes   = I.arrowToArray(this.scheme.value1); this.holes.pop()
    this.func    = this.holes.length > 0
    this.app     = this.cons == 'app' && !opts.spine
    this.bracket = opts.bracket && (this.func || this.app)
    this.outer   = opts.outer || this.bracket || this.app
    this.spine   = opts.spine || I.econs(I.appToArray(this.data)[0].value0)

    this.name = 'expr'

    this.onrenew = d => {
        this.renew(I.renewExpr(d)(this.data))
    }
    this.apply = i => d => {
        console.log('apply', i, d);
        console.log(I.fillExprWith(i)(d)(this.data));
        this.renew(I.fillExprWith(i)(d)(this.data))
    }

    this.onremove = () => {
        console.log('remove');
        this.renew(I.eempty)
    }
    this.ondrop = data => {
        // console.log('drop', data);
        this.renew(I.assignExpr(this.data)(data))
    }
  </script>
</expr>

<hole class='{opts.spine} {opts.right?"right":""} {conpact?"conpact":""}'>
  <div ref='slot' class='slot'>
    <type data={opts.data}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Droppable)

    this.name = 'expr'
    this.scheme  = I.spure(this.data)
    this.conpact = opts.conpact == null ? true : opts.conpact

    this.ondrop = d => this.renew(d)
  </script>
</hole>

<highlight class={outer: opts.outer, hover: opts.hover}>

</highlight>

<expr-emp class='term emp'>
  <type data={data}/>
  this.mixin(Mixin.Data)
</expr-emp>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</var-expr>

<app-expr class='{opts.spine} {opts.outer?"outer":""}'>
  <expr data={data.value0} spine={opts.spine} left={true} renew={renewL}/>
  <expr data={data.value1} bracket={true} right={opts.outer} renew={renewR}/>
  <script>
    this.mixin(Mixin.Data)
    this.renewL = d => this.renew(I.appC(d)(this.data.value1))
    this.renewR = d => this.renew(I.appC(this.data.value0)(d))
  </script>
</app-expr>

<num-expr class='term num'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</num-expr>

// Pattern ---------------------------------------------------------------------

<pattern class={outer: opts.outer, right: opts.right}>
  <div ref='slot' class='slot'>
    <var-pattern if={cons == 'var'} data={expr}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Selectable)

    this.expr   = opts.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = opts.data.value1
  </script>
</pattern>

<var-pattern class='term var'>
  <span class='token'>{opts.data.value0}</span>
</var-pattern>
