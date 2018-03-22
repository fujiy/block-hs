
import I from 'Block.Bridge'

// Scheme

<scheme>
  <type data={type}/>

  <script>
    this.type = opts.data.value1
  </script>
</scheme>

// Type

<type>
  <type-id      if={cons == 'id'}  data={type}/>
  <type-var     if={cons == 'var'} data={type}/>
  <type-arrow   if={cons == 'arr'} data={opts.data}/>
  <type-unknown if={cons == 'unk'} data={type}/>

  <script>
    this.type = opts.data.value0
    this.cons = I.tcons(this.type)
  </script>
</type>

<type-id class='term type'>
  <span class='token'>{opts.data.value0}</span>
</type-id>

<type-var class='term type'>
  <span class='token'>{opts.data.value0.value0}</span>
</type-var>

<type-arrow class='term type'>
  <type data={args[0]}/>
  <span class='token'>→</span>
  <type data={args[1]}/>
  <script>
    this.args = I.tappToArray(opts.data)
    this.args.shift()
  </script>
</type-arrow>

<type-unknown class='term type'>
  <span class='token'>?</span>
</type-unknown>
